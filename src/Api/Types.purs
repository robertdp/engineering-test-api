module Api.Types where

import Prelude

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), undefined)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap, over)
import Data.UUID (UUID)


type UserID = UUID

type Email = String

type Date = String

newtype User = User
  { id :: UserID
  , createdAt :: Date
  , updatedAt :: Date
  , lastLoginAt :: Date
  , firstName :: String
  , lastName :: String
  , password :: String
  , email :: Email
  , isAdmin :: Boolean
  }

derive instance newtypeUser :: Newtype User _

newtype RegisterUserRequest = RegisterUserRequest
  { first_name :: String
  , last_name :: String
  , email :: String
  , password :: String
  }

derive instance genericRegisterUserRequest :: Generic RegisterUserRequest _

instance decodeRegisterUserRequest :: Decode RegisterUserRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

newtype UserResponse = UserResponse
  { created_at :: String
  , id :: String
  , last_login_at :: String
  , updated_at :: String
  , email :: String
  , is_admin :: Boolean
  , name :: String
  }

derive instance genericUserResponse :: Generic UserResponse _

instance encodeUserResponse :: Encode UserResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

newtype ApiResponse a = ApiResponse
  { status :: Int
  , code :: Int
  , message :: NullOrUndefined String
  , result :: NullOrUndefined a
  }

derive instance newtypeApiResponse :: Newtype (ApiResponse a) _

derive instance genericApiResponse :: Generic (ApiResponse a) _

instance encodeApiResponse :: Encode a => Encode (ApiResponse a) where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

emptyApiResponse :: forall a. ApiResponse a
emptyApiResponse =
  ApiResponse
    { status : 200
    , code : 0
    , message : NullOrUndefined Nothing
    , result : NullOrUndefined Nothing
    }

makeApiResponse :: forall a. a -> ApiResponse a
makeApiResponse a =
  over ApiResponse (_ { result = NullOrUndefined $ Just a}) $ emptyApiResponse

setApiStatus :: forall a. Int -> ApiResponse a -> ApiResponse a
setApiStatus status =
  over ApiResponse $ _ { status = status }

setApiMessage :: forall a. String -> ApiResponse a -> ApiResponse a
setApiMessage message =
  over ApiResponse $ _ { message = NullOrUndefined $ Just message }

setApiCode :: forall a. Int -> ApiResponse a -> ApiResponse a
setApiCode code =
  over ApiResponse $ _ { code = code }
