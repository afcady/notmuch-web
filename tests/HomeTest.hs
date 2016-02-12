{-# LANGUAGE OverloadedStrings #-}
module HomeTest
    ( homeSpecs
    ) where

import Import hiding (get)
import Yesod.Test

homeSpecs :: YesodSpec App
homeSpecs =
  ydescribe "Basic home page tests" $
    yit "main page redirects to the login page" $ do
      get HomeR
      statusIs 303
      get ("/auth/login" :: String)
      bodyContains "Password"
