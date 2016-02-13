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
module Handler.Raw (getRawCommandR, postRawCommandR) where

import Import
import qualified Data.Text as T

rawForm :: AForm (HandlerT App IO) String
rawForm = T.unpack <$> areq textField cmd Nothing
    where cmd = FieldSettings (SomeMessage MsgRaw) Nothing (Just "Command") Nothing []

getRawCommandR :: Handler Html
getRawCommandR = defaultLayout $ do
    setTitleI MsgRaw
    ((result,widget),enctype) <- liftHandlerT $ runFormPost $ renderBootstrap3 BootstrapBasicForm rawForm
    [whamlet|
<p>_{MsgRawWarning}
<form method=post enctype=#{enctype} action=@{RawCommandR}>
  <fieldset>
    $case result
      $of FormFailure reasons
        $forall r <- reasons
          <div .alert .alert-error>#{r}
      $of _
    ^{widget}
    <input .btn .primary type=submit value=_{MsgRunCmd}>
|]

postRawCommandR :: Handler Html
postRawCommandR = do
    ((result,_),_) <- runFormPost $ renderBootstrap3 BootstrapBasicForm rawForm
    command <- case result of
        FormMissing -> invalidArgs ["Form is missing"]
        FormFailure msg -> invalidArgs msg
        FormSuccess command -> return command

    (code,out,err) <- notmuchRaw (words command)
    defaultLayout $ do
        setTitleI MsgRaw
        [whamlet|
<h1>#{command}
<p>_{MsgExitCode}: #{show code}
<h3>_{MsgStdOut}
<pre>
  #{out}
<h3>_{MsgStdErr}
<pre>
  #{err}
|]
