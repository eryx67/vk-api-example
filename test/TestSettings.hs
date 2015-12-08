{-# LANGUAGE OverloadedStrings #-}
-- |

module TestSettings where

import qualified Data.Text as T


userName :: T.Text
userName = error "please set VKontakte username in test/TestSettings.hs"

userPass :: T.Text
userPass = error "please set VKontakte password in test/TestSettings.hs"

appId :: Integer
appId = error "please set VKontakte application client id in test/TestSettings.hs"
