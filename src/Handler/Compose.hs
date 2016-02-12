{-# OPTIONS_GHC -fno-warn-orphans #-}
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
module Handler.Compose (
    getComposeR
  , postComposeR
  , getReplyR
  , getReplyAllR
  , postPreviewMessageR
) where

import Import
import Handler.ComposeFields

import Control.Arrow ((&&&), (***))
import Data.Maybe (maybeToList)
import Data.String (fromString)
import Data.Time
import Network.Mail.Mime hiding (partContent)
import System.FilePath ((</>))
import System.Locale
import System.Random (randomIO)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Markdown

import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.Conduit.List as CL
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL

-----------------------------------------------------------------------------------------
-- Compose Form
-----------------------------------------------------------------------------------------

instance Eq Address where
    (Address a1 a2) == (Address b1 b2) = a1 == b1 && a2 == b2

-- | Lookup From addresses in the settings
fromAddresses :: (MonadHandler m, HandlerSite m ~ App) => m (OptionList Address)
fromAddresses = mkOptionList <$> do
    addrs <- extraFromAddresses <$> getExtra
    forM addrs $ \a ->
        case parseAddress a of
            Left err -> do
                setMessageI err
                return $ Option ("Invalid " <> a) (Address Nothing "") ""
            Right a' -> return $ Option a a' a

-- | Parse an address header like To: and CC: into a list of address
parseAddrHeader :: (MonadHandler m, HandlerSite m ~ App) => CI.CI T.Text -> Reply -> m [Address]
parseAddrHeader hdr reply = 
    case M.lookup hdr (replyHeaders reply) of
        Nothing -> return []
        Just x  -> case parseAddresses x of
                           Left err -> do setMessageI err
                                          return []
                           Right addr -> return addr

-- | Search for the first part which is a body
findBodyText :: [MessagePart] -> [T.Text]
findBodyText [] = []
findBodyText ((MessagePart {partContent = ContentText t}):_) = map (\x -> "> " <> x <> "\n") $ T.lines t
findBodyText ((MessagePart {partContent = ContentMsgRFC822 _}):_) = []
findBodyText ((MessagePart {partContent = ContentMultipart sub}):ms) =
    case findBodyText sub of
        [] -> findBodyText ms
        x  -> x

-- | Parse a reply into a mail message and the reply body
parseReply :: (MonadHandler m, HandlerSite m ~ App) => Reply -> m (Mail, T.Text)
parseReply reply = do
    to <- parseAddrHeader "to" reply
    cc <- parseAddrHeader "cc" reply

    let extra = foldr M.delete (replyHeaders reply) ["to", "cc", "subject", "from"]

    let mail = Mail (Address Nothing "")
                    to
                    cc
                    [] -- bcc
                    [(T.encodeUtf8 $ CI.original k, v) | (k,v) <- M.toList extra]
                    [] -- parts

    tz <- liftIO getCurrentTimeZone
    let t = utcToZonedTime tz $ messageTime $ replyOriginal reply
    let ts = formatTime defaultTimeLocale "%a %b %e %R %z %Y" t

    let body = T.concat $
                [ "On "
                , T.pack ts
                , ", "
                , fromMaybe "" $ M.lookup "from" $ messageHeaders $ replyOriginal reply
                , " wrote:\n"
                ] ++ (findBodyText $ messageBody $ replyOriginal reply)

    return (mail, body)

data EmailBodyFormat = EmailBodyQuotedPrintable
                     | EmailBodyPlain
                     | EmailBodyMarkdown
  deriving (Eq, Enum, Bounded)

instance Show EmailBodyFormat where
    show EmailBodyQuotedPrintable = "Send as text/plain, UTF-8 encoded with quoted printable"
    show EmailBodyPlain = "Send as text/plain, UTF-8, no encoding"
    show EmailBodyMarkdown = "Parse body as markdown; send text and html parts"

markdownSettings :: MarkdownSettings
markdownSettings = def { msXssProtect = False -- Input is trusted from compose form
                       , msBlankBeforeBlockquote = False
                       }

-- | Read the body as markdown and create html
markdownToHtml :: T.Text -> Html
markdownToHtml = markdown markdownSettings . TL.fromStrict

-- | Create the body of the outgoing message
createBody :: (MonadHandler m, HandlerSite m ~ App)
           => EmailBodyFormat -> Textarea -> [FileInfo] -> m [Alternatives]
createBody fmt bodytext attach = do
    attachParts <- liftResourceT $ forM attach $ \f -> do
        content <- fileSource f $$ CL.consume
        return [Part (fileContentType f) Base64 (Just $ fileName f) [] (BL.fromChunks content)]

    let b = Part "text/plain; charset=UTF-8" None Nothing [] $ BL.fromChunks [T.encodeUtf8 $ unTextarea bodytext]
        bq = b { partEncoding = QuotedPrintableText }
        html = renderHtml $ markdownToHtml $ unTextarea bodytext
        hpart = Part "text/html; charset=UTF-8" QuotedPrintableText Nothing [] html

    let body = case fmt of
                EmailBodyQuotedPrintable -> [bq]
                EmailBodyPlain -> [b]
                EmailBodyMarkdown -> [bq, hpart]

    return $ body : attachParts

-- | Create a new message ID
messageID :: (MonadHandler m, HandlerSite m ~ App) => m T.Text
messageID = do t <- liftIO getCurrentTime
               let ts = formatTime defaultTimeLocale "%s" t
               i <- abs <$> liftIO (randomIO :: IO Int)
               domain <- extraMessageIDDomain <$> getExtra
               case domain of
                 "" -> return ""
                 _  -> return $ T.concat ["<notmuch-web-", T.pack ts, ".", T.pack (show i), "@", domain, ">"]

-- | Create a field settings from a string.  The default IsString instance does not set the id.
fStr :: T.Text -> FieldSettings site
fStr i = FieldSettings (fromString $ T.unpack i) Nothing (Just i) Nothing []

-- | Create a field setting from a message
fI :: AppMessage -> T.Text -> FieldSettings App
fI m i = FieldSettings (SomeMessage m) Nothing (Just i) Nothing []

-- | A helper widget to display a form element in bootstrap markup
formElem :: Bool -> FieldView App -> Widget
formElem includeHelp v = [whamlet|
<div .control-group>
    <label .control-label for=#{fvId v}>#{fvLabel v}
    <div .controls>
        ^{fvInput v}
        $if includeHelp
            <i .addr-help .icon-question-sign data-title=_{MsgAddrHelpTitle} data-content=_{MsgAddrHelp}>
|]

-- | A widget to select the address book type
addressBookWidget :: Widget
addressBookWidget = do
    $(widgetFile "abook")
    mID <- extraGoogleClientId <$> getExtra
    case mID of
        Nothing -> $(widgetFile "plain-contacts")
        Just clientID -> $(widgetFile "google-contacts")

-- | The compose form
composeForm :: Maybe Reply -> Form (Mail, Maybe FilePath)
composeForm mreply fmsg = do
    mmail <- lift $ maybe (return Nothing) (\x -> Just <$> parseReply x) mreply

    (from,fromView) <- mreq (selectField fromAddresses) (fStr "From") Nothing
    (to,toView) <- mreq addressField (fStr "To") (mailTo . fst <$> mmail)
    (cc,ccView) <- mopt addressField (fStr "CC") (Just . mailCc . fst <$> mmail)
    (bcc,bccView) <- mopt addressField (fStr "BCC") Nothing
    (subject,sView) <- mreq textField (fI MsgSubject "subj") $ M.lookup "subject" =<< (replyHeaders <$> mreply)
    (head,hView) <- mopt headerField (fI MsgExtraHeader "hdrs") (Just . mailHeaders . fst <$> mmail)
    (bfmt,fmtView) <- mreq (selectField optionsEnum) (fI MsgBodyFormat "bdyfmt") Nothing
    (body,bView) <- mreq textareaField (fI MsgBody "Body") (Textarea . snd <$> mmail)
    (attach,attachView) <- mopt multiFile (fI MsgAttach "attch") Nothing

    sendmailOpts <- map (T.pack &&& id) . extraSendmailPaths <$> getExtra
    (smail,smailView) <-
        if null sendmailOpts 
            then return (FormSuccess Nothing, Nothing)
            else (fmap Just *** Just) <$> mreq (selectFieldList sendmailOpts) (fStr "Sendmail") Nothing

    parts <- case (,,) <$> bfmt <*> body <*> attach of
               FormSuccess (f,b,a) -> FormSuccess <$> createBody f b (fromMaybe [] a)
               FormFailure err     -> return $ FormFailure err
               FormMissing         -> return $ FormMissing
               
    mid <- lift messageID

    let mkHeaders s e = ("Subject", s) : ("Message-ID", mid) : fromMaybe [] e
        mail = Mail <$> from
                    <*> to
                    <*> (fromMaybe [] <$> cc)
                    <*> (fromMaybe [] <$> bcc)
                    <*> (mkHeaders <$> subject <*> head)
                    <*> parts
        mailWithSend = (,) <$> mail <*> smail

        rightViews = [hView, fmtView] ++ maybeToList smailView
        widget = [whamlet|#{fmsg}
<div .row>
    <div .span6>
        ^{formElem False fromView}
        ^{formElem True toView}
        ^{formElem False ccView}
        ^{formElem False bccView}
    <div .span6>
        ^{addressBookWidget}
        $forall v <- rightViews
            ^{formElem False v}
^{formElem False sView}
^{formElem False bView}
^{formElem False attachView}
|]
    return (mailWithSend, widget)

previewForm :: Form T.Text
previewForm = renderDivs $ areq textField (fStr "previewtext") Nothing

-----------------------------------------------------------------------------------------
-- Handlers
-----------------------------------------------------------------------------------------

getComposeR :: Handler Html
getComposeR = do
    ((_,widget),enctype) <- runFormPost $ composeForm Nothing
    (previewWidget,previewEnctype) <- generateFormPost previewForm
    defaultLayout $ do
        setTitleI MsgCompose
        let err = [] :: [String]
        $(widgetFile "compose")

-- | Helper function for reply and reply all
replyHandler :: ReplyTo -> MessageID -> Handler Html
replyHandler rto m = do
    reply <- notmuchReply m rto
    ((_,widget),enctype) <- runFormPost $ composeForm $ Just reply
    (previewWidget,previewEnctype) <- generateFormPost previewForm
    defaultLayout $ do
        setTitleI MsgCompose
        let err = [] :: [String]
        $(widgetFile "compose")

getReplyR :: MessageID -> Handler Html
getReplyR = replyHandler ReplySender

getReplyAllR :: MessageID -> Handler Html
getReplyAllR = replyHandler ReplyAll

-- | Create a unique filename and a date header from the current time
filenameAndDate :: IO (FilePath, TL.Text)
filenameAndDate = do t <- getCurrentTime >>= utcToLocalZonedTime
                     let ts = formatTime defaultTimeLocale "%F-%T%z" t
                     let ds = formatTime defaultTimeLocale "%a, %d %b %Y %T %z" t
                     i <- randomIO :: IO Int
                     return (ts ++ "-" ++ show i, "Date: " <> TL.pack ds <> "\n")

postComposeR :: Handler Html
postComposeR = do
    ((result,widget),enctype) <- runFormPost $ composeForm Nothing
    (previewWidget,previewEnctype) <- generateFormPost previewForm
    case result of
        FormSuccess (m, smail) -> do
            msg <- liftIO $ renderMail' m
            let tmsg = TL.decodeUtf8 msg

            when production $ do
                setMessageI MsgSent
                case smail of
                    Just s -> liftIO $ sendmailCustom s ["-t"] msg
                    Nothing -> liftIO $ sendmail msg

            msentbox <- extraSentBox <$> getExtra
            case msentbox of
                Just b -> do (file, dheader) <- liftIO filenameAndDate
                             liftIO $ TL.writeFile (b </> file) $ dheader <> tmsg
                Nothing -> return ()

            defaultLayout [whamlet|<pre>#{tmsg}|]

        FormMissing -> invalidArgs ["Form is missing"]
        FormFailure err -> defaultLayout $ do
            setTitleI MsgCompose
            $(widgetFile "compose")

postPreviewMessageR :: Handler Html
postPreviewMessageR = do
    ((result,_),_) <- runFormPost previewForm
    case result of
        FormMissing -> invalidArgs ["Form is missing"]
        FormFailure err -> invalidArgs err
        FormSuccess h -> return $ markdownToHtml h
