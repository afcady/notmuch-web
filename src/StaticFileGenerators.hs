{-
Copyright (C) 2013 John Lenz <lenz@math.uic.edu>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}
module StaticFileGenerators (compressJs, compressPjax) where

import Prelude
import Yesod.EmbeddedStatic
import qualified Data.ByteString.Lazy as BL

-- | Search bytestring for */ returning the section of the bytestring up
-- to and including the */
findComment :: BL.ByteString -> BL.ByteString
findComment b = result
    where
        (untilStar, afterStar) = BL.break (==42) b -- 42 is *
        result = if "*/" `BL.isPrefixOf` afterStar
                    then untilStar `BL.append` "*/"
                    else BL.concat [untilStar, "*", findComment (BL.drop 1 afterStar)]


-- | Apply the given javascript compression tool, keeping the first comment intact.
keepFirstComment :: (BL.ByteString -> IO BL.ByteString) -> BL.ByteString -> IO BL.ByteString
keepFirstComment compressor b = do
    comp <- compressor b
    let withoutSpace = BL.dropWhile (==32) b -- 32 is space
    return $ if "/*" `BL.isPrefixOf` withoutSpace
        then findComment withoutSpace `BL.append` comp
        else comp

-- | Compress javascript
compressJs :: Bool -- ^ keep comment on yui
           -> BL.ByteString -> IO BL.ByteString
compressJs b = tryCompressTools
    [ keepFirstComment uglifyJs
    , (if b then keepFirstComment else id) yuiJavascript
    , keepFirstComment closureJs
    , keepFirstComment jasmine
    ]

compressPjax :: BL.ByteString -> IO BL.ByteString
compressPjax b = do
    lic <- BL.readFile "js/pjax-LICENSE"
    js <- compressJs True b
    return $ BL.concat ["/*", lic, "*/", js]
