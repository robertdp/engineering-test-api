module Data.JWT where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn4, runEffFn2, runEffFn4)
import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))


foreign import data JWT :: Effect

newtype Token = Token String

derive instance genericToken :: Generic Token _

instance decodeToken :: Decode Token where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance encodeToken :: Encode Token where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

foreign import signJWT_ :: forall e. EffFn2 ( jwt :: JWT | e ) String Foreign String

signJWT :: forall payload e. Encode payload => String -> payload -> Eff ( jwt :: JWT | e ) Token
signJWT key payload = do
  token <- runEffFn2 signJWT_ key (encode payload)
  pure $ Token token

foreign import verifyJWT_ :: forall payload e. EffFn4 ( jwt :: JWT | e ) (Maybe payload) (Foreign -> Maybe payload) String String (Maybe payload)

verifyJWT :: forall payload e. Decode payload => String -> Token -> Eff ( jwt :: JWT | e ) (Maybe payload)
verifyJWT key (Token token) = runEffFn4 verifyJWT_ Nothing (decode >>> runExcept >>> hush) key token
