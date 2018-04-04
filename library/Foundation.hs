{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Foundation where

import Control.Monad.Persist (PersistT, MonadPersist, SqlBackend, runSqlPoolPersistT)
import Control.Monad.Reader (asks)
import qualified Control.Monad.Persist as P
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Fluid.Server

import qualified AiCompo.Authentication.Api.Major0 as V0

import qualified DB
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Core.Types (Logger)

import qualified AiCompo.TicTacToe.Service as TS

import Import.NoFoundation
import Util

data App = App
  { appSettings :: AppSettings
  , appStatic :: Static
  , appConnPool :: ConnectionPool
  , appHttpManager :: Manager
  , appLogger :: Logger
  , appAuthPlugins :: [AuthPlugin App]
  , appTicTacToeComponents :: TS.Components
  }

data MenuItem = MenuItem
  { menuItemLabel :: Text
  , menuItemRoute :: Route App
  , menuItemAccessCallback :: Bool
  }

data MenuTypes
  = NavbarLeft MenuItem
  | NavbarRight MenuItem

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

type DB a = forall (m :: * -> *).
  (MonadIO m, Functor m) => ReaderT SqlBackend m a

persistToHandler :: PersistT SqlBackend Handler a -> Handler a
persistToHandler m = runSqlPoolPersistT m =<< asks appConnPool

instance ServiceThrower Handler
instance V0.Api'Thrower Handler

instance MonadPersist SqlBackend Handler where
  get a = persistToHandler $ P.get a
  insert a = persistToHandler $ P.insert a
  insert_ a = persistToHandler $ P.insert_ a
  insertMany a = persistToHandler $ P.insertMany a
  insertMany_ a = persistToHandler $ P.insertMany_ a
  insertEntityMany a = persistToHandler $ P.insertEntityMany a
  insertKey a b = persistToHandler $ P.insertKey a b
  repsert a b = persistToHandler $ P.repsert a b
  replace a b = persistToHandler $ P.replace a b
  delete a = persistToHandler $ P.delete a
  update a b = persistToHandler $ P.update a b
  updateGet a b = persistToHandler $ P.updateGet a b
  getJust a = persistToHandler $ P.getJust a
  belongsTo a b = persistToHandler $ P.belongsTo a b
  belongsToJust a b = persistToHandler $ P.belongsToJust a b
  insertEntity a = persistToHandler $ P.insertEntity a
  getEntity a = persistToHandler $ P.getEntity a
  getJustEntity a = persistToHandler $ P.getJustEntity a
  getBy a = persistToHandler $ P.getBy a
  deleteBy a = persistToHandler $ P.deleteBy a
  insertUnique a = persistToHandler $ P.insertUnique a
  upsert a b = persistToHandler $ P.upsert a b
  upsertBy a b c = persistToHandler $ P.upsertBy a b c
  getByValue a = persistToHandler $ P.getByValue a
  insertBy a = persistToHandler $ P.insertBy a
  insertRecord a = persistToHandler $ P.insertRecord a
  replaceUnique a b = persistToHandler $ P.replaceUnique a b
  checkUnique a = persistToHandler $ P.checkUnique a
  onlyUnique a = persistToHandler $ P.onlyUnique a
  selectFirst a b = persistToHandler $ P.selectFirst a b
  count a = persistToHandler $ P.count a
  updateWhere a b = persistToHandler $ P.updateWhere a b
  deleteWhere a = persistToHandler $ P.deleteWhere a
  selectList a b = persistToHandler $ P.selectList a b
  selectKeysList a b = persistToHandler $ P.selectKeysList a b
  deleteCascade a = persistToHandler $ P.deleteCascade a
  deleteCascadeWhere a = persistToHandler $ P.deleteCascadeWhere a
  parseMigration a = persistToHandler $ P.parseMigration a
  parseMigration' a = persistToHandler $ P.parseMigration' a
  printMigration a = persistToHandler $ P.printMigration a
  showMigration a = persistToHandler $ P.showMigration a
  getMigration a = persistToHandler $ P.getMigration a
  runMigration a = persistToHandler $ P.runMigration a
  runMigrationSilent a = persistToHandler $ P.runMigrationSilent a
  runMigrationUnsafe a = persistToHandler $ P.runMigrationUnsafe a
  rawExecute a b = persistToHandler $ P.rawExecute a b
  rawExecuteCount a b = persistToHandler $ P.rawExecuteCount a b
  rawSql a b = persistToHandler $ P.rawSql a b
  transactionSave = persistToHandler P.transactionSave
  transactionUndo = persistToHandler P.transactionUndo

instance Yesod App where
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
      Nothing -> getApprootText guessApproot app req
      Just root -> root
  makeSessionBackend _ = Just <$> defaultClientSessionBackend 120 "client_session_key.aes"
  yesodMiddleware = defaultYesodMiddleware
  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    muser <- maybeAuthPair
    mcurrentRoute <- getCurrentRoute
    (title, parents) <- breadcrumbs
    let menuItems =
          [ NavbarRight $ MenuItem
              { menuItemLabel = "Login"
              , menuItemRoute = AuthR LoginR
              , menuItemAccessCallback = isNothing muser
              }
          , NavbarRight $ MenuItem
              { menuItemLabel = "Logout"
              , menuItemRoute = AuthR LogoutR
              , menuItemAccessCallback = isJust muser
              }
          ]
    let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
    let navbarRightMenuItems = [x | NavbarRight x <- menuItems]
    let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
    let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]
    pc <- widgetToPageContent $ do
      addStylesheet $ StaticR css_bootstrap_css
      $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
  authRoute _ = Nothing
  isAuthorized (AuthR _) _ = return Authorized
  isAuthorized HomeR _ = return Authorized
  isAuthorized FaviconR _ = return Authorized
  isAuthorized RobotsR _ = return Authorized
  isAuthorized (StaticR _) _ = return Authorized
  isAuthorized ApiAuthenticationR _ = return Authorized
  isAuthorized ApiTicTacToeR _ = return Authorized
  isAuthorized DeveloperR _ = return Authorized
  isAuthorized ClientCredentialsR _ = return Authorized
  isAuthorized (ClientCredentialsEntityDeleteR _) _ = return Authorized
  isAuthorized SpaR _ = return Authorized
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
    where
      genFileName lbs = "autogen-" ++ base64md5 lbs
  shouldLog app _source level =
    appShouldLogAll (appSettings app)
      || level == LevelWarn
      || level == LevelError
  makeLogger = return . appLogger

instance YesodBreadcrumbs App where
  breadcrumb HomeR = return ("Home", Nothing)
  breadcrumb  _ = return ("home", Nothing)

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
  type AuthId App = DB.UserId
  loginDest _ = HomeR
  logoutDest _ = HomeR
  redirectToReferer _ = True
  authPlugins app = appAuthPlugins app
  authHttpManager = getHttpManager
  authenticate c = do
      mapM_ (uncurry setSession) $
        [ ("credsIdent", credsIdent c)
        , ("credsPlugin", credsPlugin c)
        ] ++ credsExtra c
      case credsPlugin c of
        "github" -> createUserWithGithub c
        _ -> error "invalid authentication provider"

createUserWithGithub :: Creds master -> Handler (AuthenticationResult App)
createUserWithGithub c = do
  now <- liftIO getCurrentTime
  pair <- P.getBy (DB.UniqueUserXGithubGithub $ credsIdent c)
  case pair of
    Just (Entity _ pair') -> do
        let userPublic' = DB.userXGithubUser pair'
        pair'' <- P.getBy $ DB.UniqueUserPublic userPublic'
        case pair'' of
          Just (Entity uid _) -> return $ Authenticated uid
          Nothing -> Authenticated <$> P.insert DB.User
            { DB.userCreated = now
            , DB.userPublic = userPublic'
            , DB.userEmail = Nothing
            , DB.userRole = DB.Role'Member
            }
    Nothing -> do
      userPublic <- liftIO $ generateText64 64
      res <- Authenticated <$> P.insert DB.User
        { DB.userCreated = now
        , DB.userPublic = userPublic
        , DB.userEmail = Nothing
        , DB.userRole = DB.Role'Member
        }
      P.insert_ DB.UserXGithub
        { DB.userXGithubCreated = now
        , DB.userXGithubUser = userPublic
        , DB.userXGithubGithub = credsIdent c
        , DB.userXGithubEmail = ""
        }
      return res

isAuthenticated :: Handler AuthResult
isAuthenticated = do
  muid <- maybeAuthId
  return $ case muid of
    Nothing -> Unauthorized "You must login to access this page"
    Just _ -> Authorized

instance YesodAuthPersist App

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
