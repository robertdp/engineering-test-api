module Main where

import Prelude

import Api.Server (runServer)
import Api.Types (ApiEffects)
import Control.Monad.Eff (Eff)


main :: forall e. Eff (ApiEffects e) Unit
main = runServer port
  where
    port = 8080
