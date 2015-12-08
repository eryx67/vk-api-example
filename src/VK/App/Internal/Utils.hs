{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
-- |

module VK.App.Internal.Utils where

import           Control.Concurrent   (forkIO)
import           Control.Monad        (forM_, void)
import qualified Data.Text            as T
import           Data.Text.Format
import qualified Data.Text.Lazy       as LT
import           Data.Typeable        (Typeable)


import           React.Flux
import           React.Flux.Lifecycle


formatDuration :: Int -> T.Text
formatDuration v =
  let ss = v `rem` 60
      ms = v `div` 60 `rem` 60
      hs = v `div` 3600
  in
    LT.toStrict $ format "{}:{}:{}" (hs, left 2 '0' ms, left 2 '0' ss)

execAction :: (a -> [SomeStoreAction])
              -> a
              -> IO ()
execAction dispatcher a =
  void . forkIO $ forM_ (dispatcher a) executeAction

txt_ :: T.Text -> ReactElementM eventHandler ()
txt_ = elemText . T.unpack

willUnmountView :: Typeable props =>
                   String -> IO () -> ReactView props
willUnmountView name action =
  defineLifecycleView name () lifecycleConfig {
    lComponentWillUnmount = Just (\_ _ -> action)
    }

willUnmountView_ :: String -> IO () -> ReactElementM eventHandler ()
willUnmountView_ name action =
  view (willUnmountView name action) () mempty

