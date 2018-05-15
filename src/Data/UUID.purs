module Data.UUID where

import Prelude (class Eq, class Ord, class Show, bind, compare, eq, pure, ($))
import Control.Monad.Eff (Eff, kind Effect)
import Data.Newtype (class Newtype)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)


foreign import data GENUUID :: Effect

newtype UUID = UUID String

derive instance newtypeUUID :: Newtype UUID _

derive instance genericUUID :: Generic UUID _

instance decodeUUID :: Decode UUID where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance encodeUUID :: Encode UUID where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

instance showUUID :: Show UUID where
  show (UUID uuid) = uuid

instance eqUUID :: Eq UUID where
  eq (UUID a) (UUID b) = eq a b

instance ordUUID :: Ord UUID where
  compare (UUID a) (UUID b) = compare a b

foreign import generateUUIDImpl :: forall e. Eff ( uuid :: GENUUID | e ) String

generateUUID :: forall e. Eff ( uuid :: GENUUID | e ) UUID
generateUUID = do
  uuid <- generateUUIDImpl
  pure $ UUID uuid
