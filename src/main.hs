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
import Prelude hiding (putStrLn, getLine)

import Application          (makeApplication)
import Control.Monad        (when)
import Crypto.PasswordStore (makePassword)
import Data.Text.Encoding   (encodeUtf8, decodeUtf8)
import Data.Text.IO         (getLine, putStrLn)
import Data.Version         (makeVersion)
import NotmuchCmd           (notmuchVersion)
import Settings             (parseExtra)
import System.Environment   (getArgs)
import System.Exit          (exitWith, ExitCode(..))
import System.IO            (hSetEcho, hFlush, stdin, stdout, hPutStrLn, stderr)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)

genPassword :: IO ()
genPassword = do
  putStr "Password: "
  hFlush stdout
  hSetEcho stdin False
  p <- getLine
  hSetEcho stdin True
  putStr "\n"
  h <- makePassword (encodeUtf8 p) 12
  putStrLn $ decodeUtf8 h

main :: IO ()
main = do
  v <- notmuchVersion
  when (v < makeVersion [0,15]) $ do
      hPutStrLn stderr "notmuch-web requires at least notmuch 0.15"
      exitWith $ ExitFailure 1

  a <- getArgs
  if "--make-password" `elem` a
    then genPassword
    else defaultMain (fromArgs parseExtra) makeApplication
