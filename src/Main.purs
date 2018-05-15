module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Node.Express.Types (EXPRESS)
import Api.Server (runServer)


main :: forall e. Eff ( ref :: REF, express :: EXPRESS | e ) Unit
main = runServer port
  where
    port = 8080
