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
{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home (
    getHomeR
  , getSearchR
  , postSearchPostR
  , getOpenSearchR
  ) where

import Import
import Handler.Tags

import Text.Hamlet (hamletFile)
import qualified Data.Text as T

searchTable :: String -> Widget
searchTable s = do
    tagHeader
    search <- notmuchSearch s
    $(widgetFile "search")

-- | The GET handler for the home page
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    folders <- extraFolders <$> getExtra
    case folders of
        ((name,search):_) -> do setTitle $ toHtml $ "Notmuch - " `T.append` name
                                searchTable search
        _ -> setTitle "Notmuch"

-- | The GET handler for the search page
getSearchR :: String -> Handler Html
getSearchR s = defaultLayout $ do
    setTitle $ toHtml $ "Notmuch - " ++ s
    searchTable s

-- | The POST handler for the search box on the navbar, just redirects to the search route.
postSearchPostR :: Handler ()
postSearchPostR = do
    s <- runInputPost $ ireq textField "q"
    redirect $ SearchR $ T.unpack s

getOpenSearchR :: Handler RepXml
getOpenSearchR = repXml <$> withUrlRenderer $(hamletFile "templates/opensearch.hamlet")
