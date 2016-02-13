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
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
module NotmuchCmd (
  -- * Search
    ThreadID(..)
  , SearchResult(..)
  , notmuchSearch

  -- * Show
  , MessageID(..)
  , MessageHeaders
  , MessageContent(..)
  , MessagePart(..)
  , Message(..)
  , messageSubject
  , messageFrom
  , Thread(..)
  , notmuchShow

  -- * Export Part
  , notmuchMessagePart

  -- * Tag
  , notmuchTagMessage
  , notmuchTagThread

  -- * Reply
  , Reply(..)
  , ReplyTo(..)
  , notmuchReply

  -- * Utils
  , notmuchRaw
  , notmuchJson
  , notmuchVersion
) where

import Prelude
import Control.Exception (Exception, throw)
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Attoparsec (sinkParser)
import Data.Conduit.Process
import Data.Monoid ((<>))
import Data.Maybe
import Data.Time.Calendar (Day(..))
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Typeable (Typeable)
import Data.Version (Version(..), parseVersion)
import Text.Blaze (ToMarkup(..))
import Text.ParserCombinators.ReadP (readP_to_S)
import System.Exit
import Yesod (PathPiece)
import Control.Monad.Trans.Resource
import Data.Conduit.ProcessOld

import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Tree as TR
import qualified Data.CaseInsensitive as CI

newtype NotmuchError = NotmuchError String
  deriving (Show,Typeable)
instance Exception NotmuchError

newtype ThreadID = ThreadID String
  deriving (Show,Read,Eq,PathPiece,FromJSON,ToJSON)

instance ToMarkup ThreadID where
    toMarkup (ThreadID s) = toMarkup s
    preEscapedToMarkup (ThreadID s) = toMarkup s

-- | A single entry returned from the notmuch search command.
data SearchResult = SearchResult {
      searchThread :: ThreadID
    , searchTime :: UTCTime
    , searchDateRel :: T.Text
    , searchSubject :: T.Text
    , searchAuthors :: T.Text
    , searchTags :: [T.Text]
    , searchMatched :: Int
    , searchTotal :: Int
    }
  deriving (Show,Eq)

instance FromJSON SearchResult where
    parseJSON (Object v) = SearchResult <$> v .: "thread"
                                        <*> (posixSecondsToUTCTime . fromInteger <$> v .: "timestamp")
                                        <*> v .: "date_relative"
                                        <*> v .:? "subject" .!= ""
                                        <*> v .:? "authors" .!= ""
                                        <*> v .: "tags"
                                        <*> v .: "matched"
                                        <*> v .: "total"
    parseJSON x = fail $ "Error parsing search: " ++ show x

instance ToJSON SearchResult where
    toJSON s = object [ "thread" .= searchThread s
                      , "time" .= searchTime s
                      , "date_relative" .= searchDateRel s
                      , "subject" .= searchSubject s
                      , "authors" .= searchAuthors s
                      , "tags" .= searchTags s
                      , "matched" .= searchMatched s
                      , "total" .= searchTotal s
                      ]

-- | The notmuch search command.
notmuchSearch :: MonadIO m => String -> m [SearchResult]
notmuchSearch s = notmuchJson $ ["search", "--format=json", "--format-version=1"] ++ words s

type MessageHeaders = M.Map (CI.CI T.Text) T.Text

data MessageContent = ContentText T.Text
                    | ContentMultipart [MessagePart]
                    | ContentMsgRFC822 [(MessageHeaders, [MessagePart])]
  deriving (Show, Eq)

data MessagePart = MessagePart {
      partID :: Int
    , partContentType :: CI.CI T.Text
    , partContentCharset :: Maybe (CI.CI T.Text)
    , partContentFilename :: Maybe T.Text
    , partContent :: MessageContent
} deriving (Show,Eq)

parseRFC822 :: V.Vector Value -> Parser MessageContent
parseRFC822 lst = ContentMsgRFC822 . V.toList <$> V.mapM p lst
    where
        p (Object o) = do h <- M.mapKeys CI.mk <$> o .: "headers"
                          b <- o .: "body"
                          return (h, b)
        p _ = fail "Invalid rfc822 body"

instance FromJSON MessagePart where
    parseJSON (Object v) = do
        i <- v .: "id"
        t <- CI.mk . T.toLower <$> v .: "content-type"
        x <- v .:? "content"
        f <- v .:? "filename"
        cs <- fmap CI.mk <$> v .:? "content-charset"
        let ctype = CI.map (T.takeWhile (/= '/')) t
        case (ctype, x) of
            ("multipart", Just (Array _)) -> MessagePart i t cs f . ContentMultipart <$> v .: "content"
            ("message", Just (Array lst)) | t == "message/rfc822" -> MessagePart i t cs f <$> parseRFC822 lst
            (_, Just (String c)) -> return $ MessagePart i t cs f $ ContentText c
            (_, Just _) -> return $ MessagePart i t cs f $ ContentText $ "Unknown content-type: " <> CI.original t
            (_, Nothing) -> return $ MessagePart i t cs f $ ContentText ""

    parseJSON x = fail $ "Error parsing part: " ++ show x

newtype MessageID = MessageID { unMessageID :: String }
  deriving (Show,Read,Eq,PathPiece,FromJSON)

data Message = Message {
      messageId :: MessageID
    , messageDateRel :: T.Text
    , messageTime :: UTCTime
    , messageHeaders :: MessageHeaders
    , messageBody :: [MessagePart]
    , messageExcluded :: Bool
    , messageMatch :: Bool
    , messageTags :: [T.Text]
    , messageFilename :: FilePath
} deriving (Show,Eq)

messageSubject :: Message -> T.Text
messageSubject Message {messageHeaders = h} =
    fromMaybe "" $ M.lookup "subject" h

messageFrom :: Message -> T.Text
messageFrom Message {messageHeaders = h} =
    fromMaybe "" $ M.lookup "from" h

instance FromJSON Message where
    parseJSON (Object v) = Message <$> v .: "id"
                                   <*> v .: "date_relative"
                                   <*> (posixSecondsToUTCTime . fromInteger <$> v .: "timestamp")
                                   <*> (M.mapKeys CI.mk <$> v .: "headers")
                                   <*> v .: "body"
                                   <*> v .: "excluded"
                                   <*> v .: "match"
                                   <*> v .: "tags"
                                   <*> v .: "filename"
    parseJSON (Array _) = return $ Message (MessageID "") "" defTime M.empty [] True False [] ""
        where defTime = UTCTime (ModifiedJulianDay 0) 0
    parseJSON x = fail $ "Error parsing message: " ++ show x

data Thread = Thread { threadForest :: TR.Forest Message }
  deriving (Show)

instance FromJSON Thread where
    parseJSON (Array vs) = Thread <$> (mapM parseTree $ V.toList vs)
    parseJSON _ = fail "Thread is not an array"

parseTree :: Value -> Parser (TR.Tree Message)
parseTree vs@(Array _) = do
    (msg, Thread t) <- parseJSON vs
    return $ TR.Node msg t
parseTree _ = fail "Tree is not an array"

-- | The notmuch show command.
notmuchShow :: MonadIO m => ThreadID -> m Thread
notmuchShow (ThreadID t) = do
    ts <- notmuchJson ["show", "--format=json", "--format-version=1", "thread:" ++ t]
    return $ Thread $ concat $ map threadForest ts

notmuchMessagePart :: (MonadIO m, MonadResource m')
                   => MessageID -> Int -> (m MessagePart, Source m' ByteString)
notmuchMessagePart (MessageID m) num = (msg, sourceProcess process)
  where
    msg = notmuchJson ["show", "--format=json", "--format-version=1", "--part=" ++ show num, "id:" ++ m]
    process = proc "notmuch" ["show", "--format=raw", "--part=" ++ show num, "id:" ++ m]

data Reply = Reply {
      replyHeaders :: M.Map (CI.CI T.Text) T.Text
    , replyOriginal :: Message
} deriving (Show,Eq)

instance FromJSON Reply where
    parseJSON (Object v) = Reply <$> (M.mapKeys CI.mk <$> v .: "reply-headers")
                                 <*> v .: "original"
    parseJSON x = fail $ "Error parsing reply: " ++ show x

data ReplyTo = ReplyAll | ReplySender
  deriving (Eq, Show)

notmuchReply :: MonadIO m => MessageID -> ReplyTo -> m Reply
notmuchReply (MessageID m) r = notmuchJson $ ["reply", "--format=json", "--format-version=1"] ++ rto ++ i
  where
    rto = case r of
            ReplyAll -> ["--reply-to=all"]
            ReplySender -> ["--reply-to=sender"]
    i = ["id:" ++ m]

notmuchTag :: MonadIO m => [String] -- ^ new tags
                        -> [String] -- ^ remove tags
                        -> String   -- ^ Search string
                        -> m ()
notmuchTag new remove search = do
    let args = "tag" : map ('+':) new ++ map ('-':) remove ++ words search
    (exit,_,err) <- notmuchRaw args
    unless (exit == ExitSuccess) $ throw (NotmuchError err)

notmuchTagMessage :: MonadIO m => [String] -- ^ new tags
                               -> [String] -- ^ remove tags
                               -> MessageID
                               -> m ()
notmuchTagMessage new remove (MessageID m) = notmuchTag new remove $ "id:" ++ m

notmuchTagThread :: MonadIO m => [String] -- ^ new tags
                              -> [String] -- ^ remove tags
                              -> ThreadID
                              -> m ()
notmuchTagThread new remove (ThreadID t) = notmuchTag new remove $ "thread:" ++ t

-- | Run a raw notmuch command.
notmuchRaw :: MonadIO m => [String] -> m (ExitCode, String, String) -- ^ exitcode, stdout, stderr
notmuchRaw args = liftIO $ readProcessWithExitCode "notmuch" args ""

-- | A helper function to run notmuch and parse the result from json.  For this
-- to work, the arguments must include '--format=json'.
notmuchJson :: (MonadIO m, FromJSON a)
            => [String]       -- ^ Arguments
            -> m a
notmuchJson args = liftIO $ do
    let process = proc "notmuch" args

    v <- runResourceT $ sourceProcess process $$ sinkParser json
    case fromJSON v of
        Error e -> throw $ NotmuchError $ "Error parsing for " ++ show args ++ " : " ++ e
        Success x -> return x

-- | The version of notmuch
notmuchVersion :: MonadIO m => m Version
notmuchVersion = do
    out <- liftIO $ readProcess "notmuch" ["--version"] ""
    let fixTags :: Char -> Char
        fixTags '+' = '-'
        fixTags '~' = '-'
        fixTags c   = c
    let vStr = map fixTags $ words out !! 1
    let vs = filter (\(_,r) -> r == "") $ readP_to_S parseVersion vStr
    case vs of
        ((v,_):_) -> return v
        _ -> throw $ NotmuchError $ "Unable to parse version: " ++ vStr
