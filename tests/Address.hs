{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Address (addrSpecs) where

import Import hiding (check)

import Handler.ComposeFields
import Network.Mail.Mime (Address(..))
import Test.Hspec
import Test.HUnit
import qualified Data.Text as T

instance Show Address where
    show = T.unpack . showAddress
instance Eq Address where
    (Address x1 y1) == (Address x2 y2) = x1 == x2 && y1 == y2

check :: T.Text -> [(T.Text, T.Text)] -> Assertion
check a expected = parsed @?= map build expected
    where
        mparsed = parseAddresses a
        parsed = case mparsed of
                  Left m -> error $ T.unpack $ renderMessage (undefined :: App) [] m
                  Right ps -> ps
        build ("",y) = Address Nothing y
        build (x,y) = Address (Just x) y

addrSpecs :: Spec
addrSpecs =
    describe "Parsing email addresses" $ do
      it "empty address" $
          check "" []
      it "single address" $
          check "abc@example.com" [("","abc@example.com")]
      it "multiple addresses" $
          check "abc@a.com, def@a.com" [("","abc@a.com"),("","def@a.com")]
      it "name and address" $
          check "John <john@a.com>" [("John", "john@a.com")]
      it "name in quotes" $
          check " \"John\" <a@b.com>" [("John", "a@b.com")]
      it "name in quotes with comma" $
          check "\"Armstrong, Niel\" <ff@ss.com>" [("Niel Armstrong", "ff@ss.com")]
      it "quotes and commas" $
          check "\"Armstrong, Niel, A\" <aa@ss.com>, bare@tt.com, John <john@ss.com> , \"Wow\" <ww@qwe.com>"
                [("Niel A Armstrong", "aa@ss.com"), ("","bare@tt.com"),
                 ("John", "john@ss.com"), ("Wow", "ww@qwe.com")]
