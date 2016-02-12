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
module StaticFiles where

import Prelude
import Yesod.EmbeddedStatic
import StaticFileGenerators

#ifdef DEVELOPMENT
#define DEV_BOOL True
#else
#define DEV_BOOL False
#endif
mkEmbeddedStatic DEV_BOOL "embStatic"
    [ embedDir "static"

    -- Jquery
    , concatFilesWith "js/jquery.js" (compressJs False) -- yui correctly detects and keeps the comment
        ["js/jquery-1-10-2.js"]

    -- Bootstrap
    , concatFilesWith "js/bootstrap.js" (compressJs True)
        ["js/bootstrap-2-3-2.js"]

    -- Select 2
    , concatFilesWith "js/select2.js" (compressJs True)
        ["js/select2-3-4-3.js"]

    -- Pjax
    , concatFilesWith "js/jquery-pjax.js" compressPjax
        ["js/jquery-pjax-1-7-3.js"]
    ]
