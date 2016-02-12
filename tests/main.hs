{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Settings
import Test.Hspec
import Yesod.Default.Config
import Yesod.Test
import Application (makeFoundation)

import HomeTest
import Address

main :: IO ()
main = do
    conf <- loadConfig $ (configSettings Testing) { csParseExtra = parseExtra }
    foundation <- makeFoundation conf
    hspec $ do
        yesodSpec foundation $
          homeSpecs
        addrSpecs
