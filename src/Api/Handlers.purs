module Api.Handlers where

import Prelude

import Api.Persistence (Store, findUser, listUsers)
import Api.Types (ApiResponse, User(User), UserResponse(UserResponse), emptyApiResponse, makeApiResponse, setApiMessage, setApiStatus)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Ref (REF)
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (encodeJSON)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.UUID (UUID(..))
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Request (getRouteParam)
import Node.Express.Response (send, setContentType, setStatus)
import Node.Express.Types (EXPRESS)


sendResponse :: forall a e. Encode a => HandlerM ( express :: EXPRESS, ref :: REF | e ) (ApiResponse a) -> Handler ( ref :: REF | e )
sendResponse handler = do
  response <- handler
  setStatus $ _.status $ unwrap response
  setContentType "application/json"
  send $ encodeJSON response

handleAff :: forall e a. Aff e a -> HandlerM e a
handleAff aff = HandlerM \_ _ _ -> aff

registerUser :: forall e. Store -> Handler ( ref :: REF | e )
registerUser store = sendResponse do
  pure $ emptyApiResponse :: ApiResponse String

getUser :: forall e. Store -> Handler ( ref :: REF | e )
getUser store = sendResponse do
  getRouteParam "id" >>=
    maybe badRequest \id ->
      handleAff $ findUser store (UUID id) >>=
          maybe notFound \user ->
            pure $ makeApiResponse $ mapUserToResponse user

getUsers :: forall e. Store -> Handler ( ref :: REF | e )
getUsers store = sendResponse $ handleAff do
  users <- listUsers store
  pure $ makeApiResponse $ mapUserToResponse <$> users


badRequest :: forall a f. Encode a => Applicative f => f (ApiResponse a)
badRequest =
  emptyApiResponse
    # setApiStatus 304
    # setApiMessage "Bad request"
    # pure

notFound :: forall a f. Encode a => Applicative f => f (ApiResponse a)
notFound =
  emptyApiResponse
    # setApiStatus 404
    # setApiMessage "Not found"
    # pure


mapUserToResponse :: User -> UserResponse
mapUserToResponse (User user) =
  UserResponse
    { created_at : user.createdAt
    , id : unwrap user.id
    , last_login_at : user.lastLoginAt
    , updated_at : user.updatedAt
    , email : user.email
    , is_admin : user.isAdmin
    , name : user.firstName <> " " <> user.lastName
    }
