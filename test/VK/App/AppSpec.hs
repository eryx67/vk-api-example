{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
-- |

module VK.App.AppSpec (main, spec) where

import           Control.Concurrent     (threadDelay)
import           Control.Monad.IO.Class (liftIO)
import           Data.List              (find)
import qualified Data.Text              as T
import           System.Directory       (getCurrentDirectory)
-- import           Test.Hspec
import           Test.Hspec.WebDriver   hiding (BrowserDefaults, Chrome)
import qualified Test.WebDriver         as W

import           TestSettings

main :: IO ()
main = hspec spec

data Browsers = Chrome
              deriving (Show, Eq, Bounded, Enum)

instance TestCapabilities  Browsers where
  newCaps Chrome =
    let copts = ["--disable-web-security"]
    in
      return W.defaultCaps{W.browser = W.chrome{W.chromeOptions = copts} }

instance Using Browsers where
  type UsingList Browsers = [Browsers]
  using d s = ([d], s)

instance Using [Browsers] where
  type UsingList [Browsers] = [Browsers]
  using d s = (d, s)

spec :: Spec
spec = session "VK application tests" $ using Chrome $ do
  it "login to Vkontakte with user credentials" $ runWD $ do
    dir <- liftIO getCurrentDirectory
    openPage $ "file://" ++ dir ++ "/example/vk.html"
    cw <- getCurrentWindow
    findElem (ByCSS "div.authorization > div.panel-body > a.btn") >>= click
    liftIO $ threadDelay 3000000
    ws <- windows
    length ws `shouldBe` 2
    let Just vkW = find (/= cw) ws
    focusWindow vkW
    findElem (ByName "email") >>= sendKeys userName
    findElem (ByName "pass") >>= sendKeys userPass
    findElem (ByCSS "form input.button") >>= click
    authUrl <- getCurrentURL
    closeWindow vkW

    focusWindow cw
    findElem (ByCSS "input.form-control") >>= sendKeys (T.pack authUrl)
    liftIO $ threadDelay 3000000
    findElem (ByCSS "button") >>= click
    liftIO $ threadDelay 3000000

  it "selects \"AnyAudio\"" $ runWD $ do
    findElem (ByCSS "a[href=\"#/audios/set-audio-selector/any-audio\"]") >>= click
    liftIO $ threadDelay 3000000
    pagerEls <- findElems (ByCSS "a[href^=\"#/audios/get-audio/\"]")
    length pagerEls `shouldBe` 11

    activeEls <- findElems (ByCSS "li.active a[href=\"#\"]")
    length activeEls `shouldBe` 1

    -- inspectSession
