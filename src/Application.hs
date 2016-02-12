{-
Copyright (C) 2013 John Lenz <lenz@math.uic.edu>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import StaticFiles (embStatic)
import System.Environment (lookupEnv)
import Yesod.Auth (getAuth)
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import qualified Data.Text as T
import qualified Network.BSD as NS

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Abook
import Handler.Compose
import Handler.Home
import Handler.Pager
import Handler.Raw
import Handler.Tags
import Handler.View

#if MIN_VERSION_http_conduit(2,0,0)
import Network.HTTP.Conduit (newManager, conduitManagerSettings)
#else
import Network.HTTP.Conduit (newManager, ManagerSettings, def)
conduitManagerSettings :: ManagerSettings
conduitManagerSettings = def
#endif

mkYesodDispatch "App" resourcesApp

makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
    logWare   = if development then logStdoutDev
                               else logStdout

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager conduitManagerSettings

    -- Load message ID domain if it has not been set
    conf' <- case (extraMessageIDDomain $ appExtra conf) of
              "" -> do u <- NS.getHostName
                       return $ conf { appExtra = (appExtra conf) { extraMessageIDDomain = T.pack u} }
              _  -> return conf

    -- Load google client id if it has not been set
    conf'' <- case (extraGoogleClientId $ appExtra conf') of
                Nothing -> do mi <- lookupEnv "NOTMUCH_WEB_GOOGLE_CLIENTID"
                              case mi of
                                 Just i -> return $ conf' { appExtra = (appExtra conf') { extraGoogleClientId = Just $ T.pack i } }
                                 Nothing -> return conf'
                Just _ -> return conf'

    return $ App conf'' embStatic manager $ extraHashedPwd $ appExtra conf''

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
