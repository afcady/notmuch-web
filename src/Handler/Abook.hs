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
module Handler.Abook (getAbookQueryR) where

import Import
import Control.Exception.Lifted (try)
import Data.Conduit.Process
import System.Exit (ExitCode(..))
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Text as C

-- | Abook returns an entry as "<email addr><tab><name><tab>".  This function parses
-- such a line into a JSON value.
parseLine :: T.Text -> Value
parseLine x = case T.split (=='\t') x of
                [] -> Null
                [n] -> String n
                (email:name:_) -> String $ T.concat [name, " <", email, ">"]

-- | Query abook for the given string
getAbookQueryR :: Handler Value
getAbookQueryR = do
    q <- fromMaybe "" <$> lookupGetParam "q"
    let p = proc "abook" ["--mutt-query", T.unpack q]
    rs <- try $ runResourceT $ sourceProcess p $= C.decode C.utf8 $= C.lines $$ C.consume
    return $ case rs of
        Left (ExitFailure 1) -> Null -- exit code 1 is for no results
        Left e -> object [ "error" .= show e ]
        Right [] -> Null
        Right [_] -> Null -- first line is a blank line
        Right (_:qs) -> Array $ V.fromList $ map parseLine qs
