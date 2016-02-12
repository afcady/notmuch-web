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
{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Handler.View(
    threadWidget
  , threadHeader
  , getThreadR
  , getMessagePartR
) where

import Import
import FilterHtml
import StaticFiles
import Handler.Tags

import Blaze.ByteString.Builder (fromByteString)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Markdown
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Text as C
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Tree as TR

#ifdef USE_ICU
import qualified Data.Text.ICU.Convert as ICU
#endif

decodePart :: (MonadLogger m, MonadHandler m) 
           => Maybe (CI.CI T.Text) -> Source (ResourceT IO) B.ByteString -> m TL.Text
decodePart charset src = case charset of
                            Just "ISO-8859-1" -> decodeConduit C.iso8859_1
                            Just "UTF-8"      -> decodeConduit C.utf8
#ifdef USE_ICU
                            Just x            -> decodeICU $ CI.original x
#endif
                            _                 -> decodeConduit C.utf8

  where decodeConduit c = TL.fromChunks <$> liftResourceT (src $= C.decode c $$ C.consume)

#ifdef USE_ICU
        decodeICU x = do $(logInfo) ("Decoding using ICU: " `T.append` x)
                         raw <- liftResourceT (src $$ C.consume)
                         c <- liftIO $ ICU.open (T.unpack x) (Just True)
                         return $ TL.fromStrict $ ICU.toUnicode c $ B.concat raw
#endif

messagePart :: MessageID -> Bool -> MessagePart -> Widget

messagePart mid _ p@(MessagePart {partContentType = "text/html"}) = do
    let ((_ :: IO MessagePart), src) = notmuchMessagePart mid $ partID p
    html <- TL.toStrict <$> decodePart (partContentCharset p) src
    [whamlet|
<div .message-part .message-html>
    #{preEscapedToMarkup $ filterHtml html}
|]

messagePart mid _ m@(MessagePart {partContent = ContentText ""}) = [whamlet|
<div .message-part .message-attachment>
  <p>
    <a href="@{MessagePartR mid (partID m)}">
      $case partContentFilename m
          $of Just f
              #{f}
          $of Nothing
              <span>No filename
    (#{CI.original $ partContentType m})
|]

-- Text which is part of an alternative
messagePart _ True (MessagePart {partContent = ContentText txt}) = [whamlet|
<div .message-part .message-text>
  <pre>
    #{txt}
|]

-- Text not part of an alternative
messagePart _ False (MessagePart {partContent = ContentText txt}) = do
    let html = TL.toStrict $ renderHtml $ markdown markdownSettings $ TL.fromStrict txt
    htmlId <- newIdent
    txtId <- newIdent
    [whamlet|
<div .message-part .message-markdown>
    <ul .nav .nav-tabs>
        <li .text-as-markdown>
            <a data-toggle=tab data-target="##{htmlId}">Markdown
        <li .active>
            <a data-toggle=tab data-target="##{txtId}">Text
    <div .tab-content>
        <div .tab-pane ##{htmlId}>
            #{preEscapedToMarkup $ filterHtml html}
        <div .tab-pane .active ##{txtId}>
            <pre>
                #{txt}
|]


messagePart _ _ (MessagePart {partContent = ContentMultipart []}) = return ()

messagePart mid _ (MessagePart {partContentType = "multipart/alternative", partContent = ContentMultipart (alternatives@(a:_))}) = do
    -- Search for html part.  If none, use the first alternative
    let active = maybe a id $ find (\x -> partContentType x == "text/html") alternatives
        isActive p = partID active == partID p
    ids <- mapM (const newIdent) alternatives
    [whamlet|
<div .message-part .message-alternative>
    <ul .nav .nav-tabs>
        $forall (i,p) <- zip ids alternatives
            <li :isActive p:.active>
                <a data-toggle=tab data-target="##{i}">#{CI.original $ partContentType p}
    <div .tab-content>
        $forall (i,p) <- zip ids alternatives
            <div .tab-pane :isActive p:.active ##{i}>
                ^{messagePart mid True p}
|]

messagePart mid _ (MessagePart {partContent = ContentMultipart parts}) =
    [whamlet|
<div .message-part .message-multipart>
    $forall p <- parts
        ^{messagePart mid False p}
|]

messagePart mid _ (MessagePart {partContent = ContentMsgRFC822 lst}) =
    [whamlet|
<div .message-part .message-rfc822>
    $forall (headers,parts) <- lst
        <div .message-rfc822-entry>
            <dl .dl-horizontal>
                $forall (h,v) <- M.toList headers
                    <dt>#{CI.original h}
                    <dd>#{v}
            $forall part <- parts
                ^{messagePart mid False part}
|]

messageWidget :: Message -> Widget
messageWidget (Message {..}) = [whamlet|
<div .message data-notmuch-message-id="#{unMessageID messageId}">
  <dl .dl-horizontal>
    $forall (h,v) <- M.toList messageHeaders
      <dt>#{CI.original h}
      <dd>#{v}
  $forall part <- messageBody
    ^{messagePart messageId False part}
|]

messages :: TR.Forest Message -> Widget
messages [] = return ()
messages forest = do
    ids <- replicateM (length forest) newIdent
    let forestWithIds = zip forest ids
    let isUnread m = "unread" `elem` messageTags m
    $(widgetFile "thread")

threadWidget :: ThreadID -> Widget
threadWidget t = do
    thread <- notmuchShow t

    let msubject = case thread of
                        (Thread ((TR.Node m _):_)) -> Just $ messageSubject m
                        _ -> Nothing
    case msubject of
        Just s -> setTitle $ toHtml s
        Nothing -> return ()

    [whamlet|
$maybe s <- msubject
    <div .page-header>
        <h3>#{s}
<div #messageThread data-notmuch-threadid=#{t}>
    ^{messages (threadForest thread)}
|]

-- | The header code for displaying threads, should only be included once in the page
threadHeader :: Widget
threadHeader = do
    tagHeader
    $(widgetFile "thread-header")

getThreadR :: ThreadID -> Handler Html
getThreadR t = defaultLayout $ do
    pjax <- isPjax
    unless pjax threadHeader
    threadWidget t

getMessagePartR :: MessageID -> Int -> Handler TypedContent
getMessagePartR mid part = do
    let (getMsg, rawMsg) = notmuchMessagePart mid part
    msg <- getMsg

    case partContentFilename msg of
        Just f ->
            addHeader "Content-Disposition"
                      ("attachment;filename=\"" <> f <> "\"")
        Nothing -> return ()

    let contenttype = T.encodeUtf8 $ CI.original $ partContentType msg
    let source = rawMsg $= C.map (Chunk . fromByteString)

    respondSource contenttype source

markdownSettings :: MarkdownSettings
markdownSettings = def { msLinkNewTab = True
                       , msXssProtect = False -- We protect using our own filtering in FilterHtml.hs
                       , msBlankBeforeBlockquote = False
                       }
