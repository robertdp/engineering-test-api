module Api.Server where

import Prelude

import Api.Handlers (getUsers, registerUser)
import Api.Middleware.BodyParser (jsonBodyParser, urlencodedBodyParser)
import Api.Persistence (Store, createStore)
import Api.Types (ApiEffects)
import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Node.Express.App (AppM, all, get, listenHttp, post, useExternal)
import Node.Express.Response (end, sendJson, setStatus)


runServer :: forall e. Int -> Eff (ApiEffects e) Unit
runServer port = launchAff_ do
  store <- createStore
  let app' = app store
  liftEff $ listenHttp app' port \_ ->
    log $ "Listening on port " <> show port

app :: forall e. Store -> AppM (ApiEffects e) Unit
app store = do
  useExternal jsonBodyParser
  useExternal urlencodedBodyParser

  post  "/register" $ registerUser store
  post  "/login"    $ registerUser store
  get   "/users"    $ getUsers store

  all "/*" do
    setStatus 404
    sendJson { code: 13, message: "Not found", status: 404 }
    end
