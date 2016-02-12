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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Foundation where

import Prelude

import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Control.Applicative
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Data.Monoid (mappend)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Conduit (Manager)
import Network.Wai (requestHeaders)
import NotmuchCmd (ThreadID, MessageID)
import Settings (widgetFile, Extra (..), development)
import StaticFiles
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod
import Yesod.Auth
import Yesod.Default.Config
import Yesod.EmbeddedStatic
import qualified Crypto.PasswordStore as PS

data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: EmbeddedStatic
    , httpManager :: Manager
    , passwordHash :: ByteString -- ^ hashed password from "Crypto.PasswordStore"
    }

mkMessage "App" "messages" "en"

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Require authentication for most routes
    isAuthorized (AuthR _) _   = return Authorized
    isAuthorized RobotsR _     = return Authorized
    isAuthorized FaviconR _    = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized _ _ = do
        mauth <- maybeAuthId
        case mauth of
          Nothing -> return AuthenticationRequired
          Just _ -> return Authorized

    authRoute _ = Just $ AuthR LoginR

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend 120 "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        let folders = extraFolders $ appExtra $ settings master
        sourceLink <- extraSourceLink <$> getExtra

        pjax <- isPjax
        if pjax
          then do pc <- widgetToPageContent widget
                  giveUrlRenderer $ pageBody pc

          else do pc <- widgetToPageContent $ do
                          addStylesheet $ StaticR css_bootstrap_min_css
                          addStylesheet $ StaticR css_bootstrap_responsive_min_css
                          addScript $ StaticR js_jquery_js
                          addScript $ StaticR js_bootstrap_js

                          let newFrm extra = do (_,x) <- mreq hiddenField "" $ Just ("new" :: String)
                                                return (FormMissing, [whamlet|#{extra} ^{fvInput x}|])
                          (newWidget,newEnctype) <- liftHandlerT $ generateFormPost newFrm

                          $(widgetFile "default-layout")

                  giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The opensearch.xml file must include {searchTerms} in the url template, but
    -- the default url renderer percent encodes the braces which doesn't work.  So we
    -- need to directly render just this single example, leaving the rest of the search
    -- rotues to the default renderer.
    urlRenderOverride y (SearchR "{searchTerms}") = Just url
        where
          emptysearch = uncurry (joinPath y (appRoot $ settings y)) $ renderRoute $ SearchR " "
          url = emptysearch `mappend` fromText "{searchTerms}"

    urlRenderOverride _ _ = Nothing

    addStaticContent = embedStaticContent getStatic StaticR mini
        where mini = if development then Right else minifym

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: (MonadHandler m, HandlerSite m ~ App) => m Extra
getExtra = fmap (appExtra . settings) getYesod

-- | Checks if the request comes from pjax
isPjax :: (MonadHandler m, HandlerSite m ~ App) => m Bool
isPjax = do r <- waiRequest
            return $ isJust $ lookup "X-PJAX" $ requestHeaders r

loginForm :: AForm (HandlerT App IO) ByteString
loginForm = encodeUtf8 <$> areq passwordField pwd Nothing
  where pwd = FieldSettings (SomeMessage MsgPassword) Nothing (Just "Password") Nothing []

instance YesodAuth App where
    type AuthId App = Text
    loginDest _  = HomeR
    logoutDest _ = HomeR
    getAuthId (Creds _ n _) = return $ Just n
    authPlugins _ = [passwordPlugin]
    authHttpManager _ = error "Manager not needed"
    maybeAuthId = lookupSession "_ID"

passwordPlugin :: AuthPlugin App
passwordPlugin = AuthPlugin "password" dispatch loginWidget
    where dispatch "POST" ["login"] = postLoginR >>= sendResponse
          dispatch _ _ = notFound

          loginR = AuthR (PluginR "password" ["login"])

          loginWidget _ = do
              ((_,widget),enctype) <- liftHandlerT $ runFormPostNoToken $ renderDivs loginForm
              [whamlet|
<form method=post enctype=#{enctype} action=@{loginR}>
    ^{widget}
    <input type=submit value=_{MsgLogin}>
|]

          postLoginR = lift $ do
              ((result,_),_) <- runFormPostNoToken $ renderDivs loginForm
              case result of
                  FormMissing -> invalidArgs ["Form is missing"]
                  FormFailure msg -> invalidArgs msg
                  FormSuccess pwd -> do
                      hash <- passwordHash <$> getYesod
                      if PS.verifyPassword pwd hash
                        then setCreds True $ Creds "password" "notmuch" []
                        else permissionDenied "Invalid password"
