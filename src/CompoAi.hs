module CompoAi where

import Control.Monad.IO.Class
import Fluid.Types
import Fluid.Server.Scotty
import Fluid.Server
import Data.Text.Lazy
import System.Environment
import Safe (readMay)
import Data.Maybe (fromMaybe)

import CompoAi.Api.TicTacToe.Server ()
import CompoAi.Api.TicTacToe.Major0

newtype App a = App { unApp :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow)

instance ServiceThrower App
instance TicTacToe'Thrower App

instance TicTacToe'Service Meta App where
  ticTacToe'Hello _ Hello{helloTarget} = return $ "Hello, " `mappend` helloTarget

main :: IO ()
main = do
  port' <- lookupEnv "PORT"
  runServer ticTacToe'pull { port = fromMaybe (port ticTacToe'pull) (readMay =<< port') } unApp routes

routes :: ScottyT Text App ()
routes = do
  ticTacToe'Scotty'Post (const defHooks) ticTacToe'pull
  ticTacToe'Scotty'Get ticTacToe'pull
