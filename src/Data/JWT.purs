module Data.JWT where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn4, runEffFn2, runEffFn4)
import Data.Foreign.Class (class Decode, class Encode)
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

foreign import signJWT_ :: forall payload e. Encode payload => EffFn2 ( jwt :: JWT | e ) String payload String

signJWT :: forall payload e. Encode payload => String -> payload -> Eff ( jwt :: JWT | e ) Token
signJWT key payload = Token <$> runEffFn2 signJWT_ key payload

foreign import verifyJWT_ :: forall payload e. Decode payload => EffFn4 ( jwt :: JWT | e ) (Maybe payload) (payload -> Maybe payload) String String (Maybe payload)

verifyJWT :: forall payload e. Decode payload => String -> Token -> Eff ( jwt :: JWT | e ) (Maybe payload)
verifyJWT key (Token token) = runEffFn4 verifyJWT_ Nothing Just key token
