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
module Import
    ( module Prelude
    , module Control.Applicative
    , module Control.Exception.Lifted
    , module Control.Monad
    , module Data.Conduit
    , module Data.List
    , module Data.Maybe
    , module Data.Monoid
    , module Foundation
    , module NotmuchCmd
    , module Settings
    , module Yesod
    ) where

#if __GLASGOW_HASKELL__ >= 706
import Prelude hiding (writeFile, readFile, head, init, last)
#else
import Prelude hiding (writeFile, readFile, head, init, last, catch)
#endif

import Foundation
import Settings
import NotmuchCmd

import Control.Applicative ((<$>), (<*>), pure, (<|>))
import Control.Exception.Lifted (catch)
import Control.Monad (void, when, forM, replicateM, unless)
import Data.Conduit
import Data.List (find)
import Data.Monoid (Monoid (mappend, mempty, mconcat), (<>))
import Data.Maybe (listToMaybe, fromMaybe, catMaybes)
import Yesod hiding (loadConfig)
