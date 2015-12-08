-- |

module VK.App (app
              , module Exp) where

import           React.Flux
import           System.IO.Unsafe (unsafePerformIO)


import           VK.App.Actions
import           VK.App.Store
import           VK.App.Types     as Exp
import           VK.App.Views
import           VK.DOM.Router    as Exp

app :: App ParentRouter
app =
  App {appName = "VK"
      , appState = store
      , appView = appView_
      , appInitAction = AppInit
      , appRouter = Just $ storeRouter appDispatcher
      }
  where
    appDispatcher action = unsafePerformIO $ do
      st <- getStoreData store
      return $ dispatch st action
