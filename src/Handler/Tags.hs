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
module Handler.Tags(
    tagHeader
  , tagWidget
  , postRetagThreadR
  , postRetagMessageR
  , postCustomRetagThreadR
  , postCustomRetagMessageR
) where

import Import
--import Data.Hashable (hash)
import qualified Data.Text as T

displayTag :: T.Text -> WidgetT App IO ()
displayTag "unread" = [whamlet|<span .label .label-important .label-tag>unread|]
displayTag t        = [whamlet|<span .label .label-info .label-tag>#{t}|]
{-
displayTag t = case hash t `mod` 5 of
                0 -> [whamlet|<span .label .label-success .label-tag>#{t}|]
                1 -> [whamlet|<span .label .label-tag>#{t}|]
                2 -> [whamlet|<span .label .label-warning .label-tag>#{t}|]
                3 -> [whamlet|<span .label .label-inverse .label-tag>#{t}|]
                _ -> [whamlet|<span .label .label-info .label-tag>#{t}|]
-}

-- | A widget consisting of the retag buttons followed by the current tag list.
tagWidget :: Either SearchResult Message -> WidgetT App IO ()
tagWidget x = do
  let url = case x of
              Left s -> RetagThreadR (searchThread s)
              Right m -> RetagMessageR (messageId m)
  let customUrl = case x of
              Left s -> CustomRetagThreadR (searchThread s)
              Right m -> CustomRetagMessageR (messageId m)

  let tags = either searchTags messageTags x

  retags <- extraRetag <$> getExtra
  [whamlet|
<span .tags>
    <span .btn-group .hide-noscript>
        $forall r <- retags
            <button .btn .retag-button .btn-link title="#{retagName r}" data-notmuch-url="@{url (retagName r)}">
                <i class="#{retagIcon r}">
        <button .btn .retag-custom-button .btn-link title=_{MsgMessageRetagging} data-notmuch-url=@{customUrl}>
            <i .icon-edit>
    $forall tag <- tags
        ^{displayTag tag}
|]

-- | When posting to retag routes, the thread and retag information
-- is avaialble in the route but we also want a CSRF token to protect
-- against CSRF attacks.  Therefore the body of the post contains
-- an empty form (internally Yesod includes the CSRF token for us).
retagForm :: Form ()
retagForm = renderDivs $ pure ()

customRetagForm :: Form (Maybe T.Text, Maybe T.Text)
customRetagForm = 
    renderBootstrap $ (,) <$> aopt textField (FieldSettings (SomeMessage MsgTagsToAdd) Nothing (Just "add") Nothing []) Nothing
                          <*> aopt textField (FieldSettings (SomeMessage MsgTagsToRemove) Nothing (Just "remove") Nothing []) Nothing

-- | A widget that contains the support elements for retagging, including the forms
-- and javascript.  This widget must be included only ONCE in the page.  A notmuch:retag
-- event is raised after a successful retag.
tagHeader :: Widget
tagHeader = do
    (retagWidget,retagEnc) <- liftHandlerT $ generateFormPost retagForm
    (customWidget,customEnc) <- liftHandlerT $ generateFormPost customRetagForm
    $(widgetFile "tag-header")

postRetagThreadR :: ThreadID -> T.Text -> Handler Value
postRetagThreadR t name = do
    ((result,_),_) <- runFormPost retagForm
    case result of
        FormMissing -> invalidArgs ["form is missing"]
        FormFailure msg -> invalidArgs msg
        FormSuccess _ -> return ()

    retags <- extraRetag <$> getExtra
    let retag = filter (\r -> retagName r == name) retags
    case retag of
        [] -> notFound
        (r:_) -> do notmuchTagThread (retagAdd r) (retagRemove r) t
                    return $ toJSON r

postRetagMessageR :: MessageID -> T.Text -> Handler Value
postRetagMessageR m name = do
    ((result,_),_) <- runFormPost retagForm
    case result of
        FormMissing -> invalidArgs ["form is missing"]
        FormFailure msg -> invalidArgs msg
        FormSuccess _ -> return ()

    retags <- extraRetag <$> getExtra
    let retag = filter (\r -> retagName r == name) retags
    case retag of
        [] -> notFound
        (r:_) -> do notmuchTagMessage (retagAdd r) (retagRemove r) m
                    return $ toJSON r

postCustomRetagThreadR :: ThreadID -> Handler Value
postCustomRetagThreadR t = do
    ((result,_),_) <- runFormPost customRetagForm
    (add, remove) <- case result of
                       FormMissing -> invalidArgs ["form is missing"]
                       FormFailure msg -> invalidArgs msg
                       FormSuccess (a,r) -> return (T.unpack <$> a, T.unpack <$> r)
    let add' = maybe [] words add
        rem' = maybe [] words remove

    notmuchTagThread add' rem' t
    return $ object [ "add" .= add', "remove" .= rem' ]

postCustomRetagMessageR :: MessageID -> Handler Value
postCustomRetagMessageR m = do
    ((result,_),_) <- runFormPost customRetagForm
    (add, remove) <- case result of
                       FormMissing -> invalidArgs ["form is missing"]
                       FormFailure msg -> invalidArgs msg
                       FormSuccess (a,r) -> return (T.unpack <$> a, T.unpack <$> r)
    let add' = maybe [] words add
        rem' = maybe [] words remove

    notmuchTagMessage add' rem' m
    return $ object [ "add" .= add', "remove" .= rem' ]
