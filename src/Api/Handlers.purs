module Api.Handlers where

import Prelude

import Api.Persistence (Store, findUser, insertUser, listUsers, updateUser)
import Api.Types (User(..), UserLoginRequest(..), UserLoginResponse(..), UserRegisterRequest(..), UserResponse(..), ApiEffects)
import Api.Validation (notEmpty, validEmail)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Either (Either(Right, Left))
import Data.Foreign (MultipleErrors, renderForeignError)
import Data.Foreign.Class (class Encode, encode)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.JSDate (now, toString)
import Data.JWT (signJWT)
import Data.List.NonEmpty (head, intercalate, toList)
import Data.Maybe (Maybe(..))
import Data.UUID (generateUUID)
import Data.Validation.Semigroup (unV)
import Node.Express.Handler (HandlerM(..))
import Node.Express.Request (getBody)
import Node.Express.Response (end, send, setContentType, setStatus)


type ApiHandler e = HandlerM (ApiEffects e) Unit

-- | This is the type that all responses need to conform to
type ApiResponse a = { code :: Int, message :: String, result :: NullOrUndefined a, status :: Int }

-- | A model of all possible responses types
data ApiResponseType a
  = Success a
  | Created a
  | BadRequest String
  | InternalServerError
  | EmailNotFound
  | PasswordDoesNotMatch
  | EmailAlreadyExists
  | NameIsEmpty
  | EmailIsEmpty
  | PasswordIsEmpty

-- | Transform an ApiResponseType into an ApiResponse and send it back to the client
sendApiResponse :: forall e a. Encode a => HandlerM (ApiEffects e) (ApiResponseType a) -> ApiHandler e
sendApiResponse handler = do
  response <- handler <#> mapResponse
  setContentType "application/json"
  setStatus response.status
  send $ response { result = encode response.result }
  end

  where
    mapResponse :: ApiResponseType a -> ApiResponse a
    mapResponse = case _ of
      Success a ->
        emptyResponse { result = NullOrUndefined $ Just a }
      Created a ->
        emptyResponse { result = NullOrUndefined $ Just a, status = 201 }
      BadRequest err ->
        emptyResponse { code = 14, message = err, status = 400 }
      InternalServerError ->
        emptyResponse { code = 14, message = "Internal server error", status = 500 }
      EmailNotFound ->
        emptyResponse { message = "User email was not found", code = 31 }
      PasswordDoesNotMatch ->
        invalidRequest { message = "User passwords do not match", code = 32 }
      EmailAlreadyExists ->
        invalidRequest { message = "User email already exists", code = 30 }
      NameIsEmpty ->
        invalidRequest { message = "User name cannot be empty", code = 2 }
      EmailIsEmpty ->
        invalidRequest { message = "User email cannot be empty", code = 3 }
      PasswordIsEmpty ->
        invalidRequest { message = "User password cannot be empty", code = 4 }

    emptyResponse :: ApiResponse a
    emptyResponse = { code: 0, message: "", result: NullOrUndefined Nothing, status: 200 }

    invalidRequest :: ApiResponse a
    invalidRequest = emptyResponse { status = 422 }

getUsers :: forall e. Store -> ApiHandler e
getUsers store = sendApiResponse $ handleAff do
  users <- listUsers store
  pure $ Success $ mapUserToResponse <$> users

registerUser :: forall e. Store -> ApiHandler e
registerUser store = sendApiResponse do
  getBody >>= case _ of
    Left err ->
      pure $ BadRequest $ renderForeignErrors err
    Right (UserRegisterRequest request') ->
      validateRequest request'
        # unV (pure <<< head) \request -> do
          user <- userFromRequest request
          result <- handleAff $ insertUser store user
          case result of
            Left _ ->
              pure EmailAlreadyExists
            Right _ ->
              pure $ Success $ mapUserToResponse user
  where
    validateRequest request =
      { first_name: _, last_name: _, email: _, password: _}
        <$> notEmpty NameIsEmpty request.first_name
        <*> notEmpty NameIsEmpty request.last_name
        <*> notEmpty EmailIsEmpty request.email
        <*> notEmpty PasswordIsEmpty request.password

    userFromRequest request = handleEff do
      uuid <- generateUUID
      currentTime <- now >>= toString >>> pure
      pure $ User
        { id : uuid
        , createdAt : currentTime
        , updatedAt : currentTime
        , lastLoginAt : currentTime
        , firstName : request.first_name
        , lastName : request.last_name
        , password : request.password
        , email : request.email
        , isAdmin : false
        }

loginUser :: forall e. Store -> ApiHandler e
loginUser store = sendApiResponse do
  getBody >>= case _ of
    Left err ->
      pure $ BadRequest $ renderForeignErrors err
    Right (UserLoginRequest request') ->
      validateRequest request'
        # unV (pure <<< head) \request -> do
          user' <- handleAff $ findUser store \(User { email }) -> email == request.email
          case user' of
            Nothing ->
              pure EmailNotFound
            Just user@(User { password }) ->
              if password == request.password then do
                updateLastLogin user
                sendToken $ mapUserToResponse user
              else
                pure PasswordDoesNotMatch
  where
    validateRequest request =
      { email: _, password: _}
        <$> validEmail EmailIsEmpty request.email
        <*> notEmpty PasswordIsEmpty request.password

    updateLastLogin ::User -> ApiHandler e
    updateLastLogin (User { id }) = do
      currentTime <- handleEff now >>= toString >>> pure
      handleAff $ updateUser store id \(User user) -> Just $ User $ user { lastLoginAt = currentTime }

    sendToken :: UserResponse -> HandlerM (ApiEffects e) (ApiResponseType UserLoginResponse)
    sendToken user = do
      token <- handleEff $ signJWT "secret key" user
      pure $ Success $ UserLoginResponse { token, user }

-- | Turn request body decoding errors into something we can send back to the client
renderForeignErrors :: MultipleErrors -> String
renderForeignErrors = toList >>> map renderForeignError >>> intercalate ", "

-- | Lift an Aff into the Express handler monad
handleAff :: forall e a. Aff e a -> HandlerM e a
handleAff a = HandlerM \_ _ _ -> a

-- | Lift an Eff into the Express handler monad
handleEff :: forall e a. Eff e a -> HandlerM e a
handleEff a = HandlerM \_ _ _ -> liftEff a

-- | Transform internal user representation to "public" representation
mapUserToResponse :: User -> UserResponse
mapUserToResponse (User user) =
  UserResponse
    { created_at : user.createdAt
    , id : user.id
    , last_login_at : user.lastLoginAt
    , updated_at : user.updatedAt
    , email : user.email
    , is_admin : user.isAdmin
    , name : user.firstName <> " " <> user.lastName
    }
