module Api.Server where

import Prelude

import Api.Handlers (getUser, getUsers, registerUser)
import Api.Persistence (Store, createStore)
import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Ref (REF)
import Node.Express.App (App, get, listenHttp, post)
import Node.Express.Types (EXPRESS)


runServer :: forall e. Int -> Eff ( ref :: REF, express :: EXPRESS | e ) Unit
runServer port = launchAff_ do
  store <- createStore
  let app' = app store
  liftEff $ listenHttp app' port \_ ->
    log $ "Listening on port " <> show port

app :: forall e. Store -> App ( ref :: REF | e )
app store = do
  post "/register" $ registerUser store
  get "/users/:id" $ getUser store
  get "/users" $ getUsers store
