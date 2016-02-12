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
module Handler.ComposeFields (
    addressField
  , parseAddress
  , parseAddresses
  , showAddress
  , headerField
  , multiFile
) where

import Import
import StaticFiles

import Data.Attoparsec.Text
import Data.String (fromString)
import Network.Mail.Mime (Address(..))

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Email.Validate as E

-- | Parse an email address in angle brackets
emailInBrackets :: Parser T.Text
emailInBrackets = do
    void $ char '<'
    y <- takeTill (=='>') <?> "Email address"
    void (char '>' <?> "Expecting '>'")
    skipSpace
    return y

quotedName :: Parser T.Text
quotedName = do
    void $ char '"'
    x <- takeTill (=='"')
    void $ char '"'
    skipSpace
    let parts = T.split (==',') x
    return $ case parts of
                (p:ps) -> T.unwords $ map T.strip $ ps ++ [p]
                [] -> ""

unquotedName :: Parser T.Text
unquotedName = T.strip <$> takeTill (\x -> x == '<' || x == ',')

-- | Parses an email address, which can either just be a direct email
-- address or can be an address in the form "Name <email>"
address :: Parser Address
address = do
    skipSpace
    x <- quotedName <|> unquotedName
    y <- Just <$> emailInBrackets <|> return Nothing
    case y of
        Just e -> return $ Address (Just x) e
        Nothing -> return $ Address Nothing x

-- | Parse a list of addresses separated by commas
addresses :: Parser [Address]
addresses = do as <- address `sepBy1` char ','
               endOfInput <?> "Expecting ',' or '>'"
               return as

-- | Checks if an email is valid
checkAddr :: Address -> Either (SomeMessage App) Address
checkAddr a@(Address _ e) | E.isValid (T.encodeUtf8 e) = Right a
checkAddr (Address _ e) = Left $ SomeMessage $ MsgInvalidEmail e

-- | Parse a single address
parseAddress :: T.Text -> Either (SomeMessage App) Address
parseAddress t = case parseOnly' address t of
                       Left err -> Left $ fromString $ concat ["Error parsing ", T.unpack t, ": ", err]
                       Right a  -> checkAddr a

-- | Parse a list of addresses separated by commas
parseAddresses :: T.Text -> Either (SomeMessage App) [Address]
parseAddresses t = case parseOnly' addresses t of
                       Left err -> Left $ fromString $ concat ["Error parsing ", T.unpack t, ": ", err]
                       Right [(Address Nothing "")] -> Right []
                       Right a -> mapM checkAddr a

showAddress :: Address -> T.Text
showAddress (Address {addressName = Just name, addressEmail = e}) = T.concat [name, " <", e, ">"]
showAddress (Address {addressName = Nothing, addressEmail = e}) = e

addrWidget :: FieldViewFunc (HandlerT App IO) [Address]
addrWidget theID name attrs val isReq = do 
    addStylesheet $ StaticR css_select2_css
    addScript $ StaticR js_select2_js

    let addrs = either id (T.intercalate "," . map showAddress) val

    [whamlet|
      <input type=text ##{theID} name=#{name} .address-field :isReq:required value="#{addrs}" *{attrs}>
    |]

addressField :: Field (HandlerT App IO) [Address]
addressField = Field
  { fieldParse = \addr _ -> case addr of
                              []    -> return $ Right Nothing
                              (a:_) -> return $ Just <$> parseAddresses a
  , fieldView = addrWidget
  , fieldEnctype = UrlEncoded
  }

-- | Parse a header
header :: Parser (B.ByteString, T.Text)
header = do
    k <- takeWhile1 (\c -> not (isEndOfLine c) && c /= ':')
    void $ char ':'
    skipSpace
    v <- takeTill isEndOfLine
    return (T.encodeUtf8 k, v)

-- | Parse a list of headers
headers :: Parser [(B.ByteString, T.Text)]
headers = sepBy header endOfLine <?> "Headers"

headerField :: Field (HandlerT App IO) [(B.ByteString,T.Text)]
headerField = Field 
    { fieldParse = \x _ -> case x of
                             [] -> return $ Right Nothing
                             ("":_) -> return $ Right Nothing
                             (n:_) -> return $ case parseOnly' headers n of
                                                 Left err -> Left $ fromString err
                                                 Right [] -> Right $ Nothing
                                                 Right h  -> Right $ Just h
    , fieldView = \theId name attrs val isReq -> do
        let hdrs = case val of
                     Left txt -> txt
                     Right vals -> T.intercalate "\n" [ T.decodeUtf8 x <> ": " <> y | (x,y) <- vals]
        [whamlet|
<textarea id=#{theId} name=#{name} *{attrs} rows=4 cols=50 wrap=off :isReq:required>
    #{hdrs}
|]
    , fieldEnctype = UrlEncoded
    }

multiFile :: Field (HandlerT master IO) [FileInfo]
multiFile = Field p view Multipart
    where
        p _ fs = return $ Right $ Just fs
        view fId name attrs _ _ = [whamlet|
<input type=file name=#{name} ##{fId} multiple *{attrs}>
|]


-- | A version of parseOnly which includes the context of the failure.
parseOnly' :: Parser a -> T.Text -> Either String a
parseOnly' p t = checkRes (parse p t)
    where checkRes result = case result of
                              Fail _ ctx err -> Left $ show ctx ++ " " ++ err
                              Partial f -> checkRes $ f ""
                              Done _ x -> Right x
