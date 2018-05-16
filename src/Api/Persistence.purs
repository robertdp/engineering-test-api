module Api.Persistence where

import Prelude

import Api.Types (User(User), UserID)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef)
import Data.Array (find, findMap, fromFoldable)
import Data.Either (Either(..))
import Data.Map (Map, empty, insert, lookup, update, values)
import Data.Maybe (Maybe(..))


-- | Crude representation of an in-memory database
type Store = Ref (Map UserID User)

type StoreM e a = Aff ( ref :: REF | e ) a

createStore :: forall e. StoreM e Store
createStore = liftEff $ newRef empty

-- | All errors that can be encountered when inserting a user
data InsertError
  = InsertEmailConflict
  | InsertIDConflict

insertUser :: forall e. Store -> User -> StoreM e (Either InsertError Unit)
insertUser store user@(User { id, email }) = do
  conflict <- findConflict
  case conflict of
    Just err ->
      pure $ Left err
    Nothing -> do
      void $ liftEff $ modifyRef store (insert id user)
      pure $ pure unit
  where
    findConflict :: StoreM e (Maybe InsertError)
    findConflict = listUsers store >>= pure <<< findMap
      \(User { id : id', email : email' }) ->
        if id == id' then
          Just InsertIDConflict
        else
          if email == email' then
            Just InsertEmailConflict
          else
            Nothing

findUserById :: forall e. Store -> UserID -> StoreM e (Maybe User)
findUserById store id = liftEff do
  readRef store >>= pure <<< lookup id

findUser :: forall e. Store -> (User -> Boolean) -> StoreM e (Maybe User)
findUser store predicate =
  listUsers store >>= find predicate >>> pure

updateUser :: forall e. Store -> UserID -> (User -> Maybe User) -> StoreM e Unit
updateUser store id f = do
  user <- findUserById store id
  case user of
    Nothing ->
      pure unit
    Just user' ->
      liftEff $ modifyRef store (update (const $ f user') id)

listUsers :: forall e. Store -> StoreM e (Array User)
listUsers store = liftEff do
  readRef store >>= pure <<< fromFoldable <<< values

