module Main where

import Prelude

import Api.Server (runServer)
import Api.Types (ApiEffects)
import Control.Monad.Eff (Eff)
import Node.Express.Types (EXPRESS)


main :: forall e. Eff (ApiEffects ( express :: EXPRESS | e )) Unit
main = runServer port
  where
    port = 8080
