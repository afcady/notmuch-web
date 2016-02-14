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

module Settings where

import Prelude

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml
import Language.Haskell.TH.Syntax (Q, Exp)
import Text.Hamlet
import Yesod.Default.Config
import Yesod.Default.Util

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

widgetFile :: String -> Q Exp
widgetFile = (if development then widgetFileReload
                             else widgetFileNoReload)
              widgetFileSettings

data RetagEntry = RetagEntry
    { retagName :: Text
    , retagIcon :: Text
    , retagAdd :: [String]
    , retagRemove :: [String]
    } deriving Show

instance ToJSON RetagEntry where
    toJSON r = object [ "name" .= retagName r
                      , "icon" .= retagIcon r
                      , "add" .= retagAdd r
                      , "remove" .= retagRemove r
                      ]
instance FromJSON RetagEntry where
    parseJSON (Object o) = RetagEntry <$> o .: "name"
                                      <*> o .: "icon"
                                      <*> o .:? "add" .!= []
                                      <*> o .:? "remove" .!= []
    parseJSON _ = fail "Retag is not an object"

data Extra = Extra
    { extraHashedPwd  :: ByteString
    , extraFolders :: [(Text, String)]
    , extraRetag :: [RetagEntry]
    , extraFromAddresses :: [Text]
    , extraSentBox :: Maybe FilePath
    , extraMessageIDDomain :: Text
    , extraGoogleClientId :: Maybe Text
    , extraSourceLink :: Text
    , extraSendmailPaths :: [FilePath]
    } deriving Show

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = do
    pwd       <- encodeUtf8 <$> o .: "hashed-password"
    fdl       <- o .: "folders" >>= mapM parseFolder
    retag     <- o .: "retag"
    from      <- o .:? "from-address" .!= "<test@example.com>"
    fromlst   <- o .:? "from-addresses"
    let f = fromMaybe [from] fromlst
    sent      <- o .:? "sent-box"
    dom       <- o .:? "message-id-domain" .!= ""
    gcid      <- o .:? "google-client-id"
    sl        <- o .:? "source-link" .!= "https://bitbucket.org/wuzzeb/notmuch-web/src"
    sendmail  <- o .:? "sendmail" .!= []
    return $ Extra pwd fdl retag f sent dom gcid sl sendmail

parseFolder :: Object -> Parser (Text, String)
parseFolder o = (,) <$> o .: "name"
                    <*> o .: "search"

development :: Bool
development =
#if DEVELOPMENT
  True
#else
  False
#endif

production :: Bool
production = not development
