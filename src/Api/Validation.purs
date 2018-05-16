module Api.Validation where

import Prelude

import Api.Types (Email)
import Data.Either (fromRight)
import Data.List.Types (NonEmptyList)
import Data.String (trim)
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Validation.Semigroup (V, invalid)
import Partial.Unsafe (unsafePartial)


type Validated a b = V (NonEmptyList a) b

unsafeRegexFromString :: String -> Regex
unsafeRegexFromString pattern =
  unsafePartial $ fromRight $ regex pattern noFlags

notEmpty :: forall a.  a -> String -> Validated a String
notEmpty err value =
  case trim value of
    "" -> invalid $ pure err
    s -> pure s

validEmail :: forall a. a -> Email -> Validated a Email
validEmail err value =
  if test emailRegex value then
    pure value
  else
    invalid $ pure err
  where
    emailRegex = unsafeRegexFromString "^[^@]+@[^@]+.[^@]+$"

equalValues :: forall a b. Eq b => a -> b -> b -> Validated a b
equalValues err a b =
  if a == b then
    pure a
  else
    invalid $ pure err
