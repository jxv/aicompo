module CompoAi where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Fluid.Types (Pull(..), defHooks, Hooks(..))
import Fluid.Server.Scotty (runServer, ScottyT)
import Fluid.Server (ServiceThrower, MonadCatch ,MonadThrow)
import qualified Data.Text.Lazy as TL
import System.Environment
import Safe (readMay)
import Data.Maybe (fromMaybe)

import qualified CompoAi.TicTacToe.Api.Server as T
import qualified CompoAi.TicTacToe.Api.Major0 as T0
import qualified CompoAi.TicTacToe.Service as TS

data Config = Config
  { _cTicTacToe :: TS.TObjects
  }

newtype App a = App { unApp :: ReaderT Config IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadReader Config)

runApp :: Config -> App a -> IO a
runApp config (App m) = runReaderT m config

instance ServiceThrower App
instance T0.TicTacToe'Thrower App

instance T0.TicTacToe'Service TS.UserId App where
  ticTacToe'PostStart meta = asks _cTicTacToe >>= (\t -> TS.postStart t meta)
  ticTacToe'PostMove meta req = asks _cTicTacToe >>= (\t -> TS.postMove t meta req)

--

main :: IO ()
main = do
  port' <- lookupEnv "PORT"
  ticTacToe <- TS.newTObjects
  let config = Config ticTacToe
  _ <- TS.forkDispatcher ticTacToe
  runServer
    T.ticTacToe'pull { port = fromMaybe (port T.ticTacToe'pull) (readMay =<< port') }
    (runApp config)
    routes

routes :: ScottyT TL.Text App ()
routes = do
  T.ticTacToe'Scotty'Post T.ticTacToe'pull (const $ defHooks {metaMiddleware = ticTacToeMetaMiddleware0})
  T.ticTacToe'Scotty'Get T.ticTacToe'pull

ticTacToeMetaMiddleware0 :: T0.AccessToken -> App TS.UserId
ticTacToeMetaMiddleware0 (T0.AccessToken accessToken') = return $ TS.UserId accessToken'