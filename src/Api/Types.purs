module Api.Types where

import Prelude

import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.JWT (JWT, Token)
import Data.UUID (GENUUID, UUID)
import Node.Express.Types (EXPRESS)


type ApiEffects e = ( express :: EXPRESS, ref :: REF, uuid :: GENUUID, now :: NOW, jwt :: JWT | e )

type UserID = UUID

type Email = String

type Date = String

-- | Internal user type
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

derive instance genericUser :: Generic User _

newtype UserRegisterRequest = UserRegisterRequest
  { first_name :: String
  , last_name :: String
  , email :: String
  , password :: String
  }

derive instance genericRegisterUserRequest :: Generic UserRegisterRequest _

instance decodeRegisterUserRequest :: Decode UserRegisterRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

newtype UserLoginRequest = UserLoginRequest
  { email :: String
  , password :: String
  }

derive instance genericUserLoginRequest :: Generic UserLoginRequest _

instance decodeUserLoginRequest :: Decode UserLoginRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

newtype UserLoginResponse = UserLoginResponse
  { token :: Token
  , user :: UserResponse
  }

derive instance genericUserLoginResponse :: Generic UserLoginResponse _

instance encodeUserLoginResponse :: Encode UserLoginResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

-- | External user type
newtype UserResponse = UserResponse
  { created_at :: String
  , id :: UUID
  , last_login_at :: String
  , updated_at :: String
  , email :: String
  , is_admin :: Boolean
  , name :: String
  }

derive instance genericUserResponse :: Generic UserResponse _

instance showUserResponse :: Show UserResponse where
  show = genericShow

instance encodeUserResponse :: Encode UserResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
