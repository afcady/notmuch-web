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

module Handler.Pager (getThreadPagerR) where

import Import
import StaticFiles
import Handler.View

import Data.Aeson (encode)
import Text.Blaze (unsafeLazyByteString)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

encodeSearch :: (Route App -> T.Text) -> [SearchResult] -> Html
encodeSearch ur search = unsafeLazyByteString $ encode $ map enc search
  where
    enc s = uncurry H.insert ("url" .= ur (ThreadR $ searchThread s)) (toObj s)
    toObj s = case toJSON s of
                  Object o -> o
                  _ -> error "Search is not an object"

getThreadPagerR :: String -> Handler Html
getThreadPagerR s = defaultLayout $ do
    search <- notmuchSearch s
    ur <- getUrlRender
    retags <- extraRetag <$> getExtra
    addScript $ StaticR js_jquery_pjax_js
    $(widgetFile "pager")
