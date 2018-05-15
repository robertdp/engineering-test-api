module Api.Persistence where

import Prelude
import Api.Types (User(User), UserID)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef)
import Data.Array (fromFoldable)
import Data.Map (Map, delete, empty, insert, lookup, update, values)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)


type Store = Ref (Map UserID User)

type StoreM e a = Aff ( ref :: REF | e ) a

createStore :: forall e. StoreM e Store
createStore = liftEff do
  newRef empty

insertUser :: forall e. Store -> User -> StoreM e Unit
insertUser store user@(User { id }) = liftEff do
  modifyRef store (insert id user)

findUser :: forall e. Store -> UserID -> StoreM e (Maybe User)
findUser store id = liftEff do
  readRef store >>= pure <<< lookup id

updateUser :: forall e. Store -> UserID -> (User -> Maybe User) -> StoreM e Unit
updateUser store id f = do
  user <- findUser store id
  case user of
    Nothing ->
      pure unit
    Just user' ->
      liftEff $ modifyRef store (update (const $ f user') id)

deleteUser :: forall e. Store -> UserID -> StoreM e Unit
deleteUser store id = liftEff do
  modifyRef store (delete id)

listUsers :: forall e. Store -> StoreM e (Array User)
listUsers store = liftEff do
  readRef store >>= pure <<< fromFoldable <<< values

modify :: forall t a. Newtype t a => (a -> a) -> t -> t
modify f = wrap <<< f <<< unwrap
