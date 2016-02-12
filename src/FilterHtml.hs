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
module FilterHtml (filterHtml) where

import Import
import Data.Text (Text)
import Text.HTML.SanitizeXSS
import Text.HTML.TagSoup

-- | Filter Html
filterHtml :: Text -> Text
filterHtml = filterTags (balanceTags . allowedTags . safeTags)

-- | List of allowed tags.
allowed :: [Text]
allowed = ["div", "p", "br", "blockquote"]

-- | Filter which tags are allowed
allowedTags :: [Tag Text] -> [Tag Text]
allowedTags [] = []
allowedTags (t@(TagOpen name _):ts)
    | name `elem` allowed = t : allowedTags ts
    | otherwise           = allowedTags ts

allowedTags (t@(TagClose name):ts)
    | name `elem` allowed = t : allowedTags ts
    | otherwise           = allowedTags ts

allowedTags (t:ts) = t : allowedTags ts
