{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
-- |

module VK.App.Widgets.Internal.IdleTimer(IdleTimer
                                        , mkIdleTimer
                                        , idleTimerSetDelay
                                        , idleTimerStart
                                        , idleTimerStop
                                        , idleTimerStartActions
                                        , idleTimerStopAction
                                        ) where

import           Control.Concurrent         (forkIO)
import           Control.Concurrent.Suspend (msDelay, usDelay)
import           Control.Concurrent.Timer
import           Control.DeepSeq
import           Control.Monad              (forM_, void)
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import           System.IO.Unsafe           (unsafePerformIO)


import           React.Flux

data IdleTimerAction =
  StartTimer ![SomeStoreAction]
  | SetDelay !Int
  | StopTimer
    deriving (Typeable, Generic, NFData)

data IdleTimer = IdleTimer {
  idleTimer   :: !TimerIO
  , idleDelay :: !Int
                   -- ^ milliseconds
  }
               deriving Typeable

instance StoreData IdleTimer where
  type StoreAction IdleTimer = IdleTimerAction

  transform action st@IdleTimer{..} = do
    case action of
      StartTimer actions -> do
        let delay = msDelay $ fromIntegral idleDelay
        void $ oneShotStart idleTimer
          (void . forkIO $ forM_ actions executeAction) delay
        return st
      SetDelay v -> do
        return st{idleDelay = v}
      StopTimer -> do
        void $ oneShotStart idleTimer (return ()) (usDelay 1)
        return st

mkIdleTimer :: Int -> ReactStore IdleTimer
mkIdleTimer delay = unsafePerformIO $ do
  tm <- newTimer
  return . mkStore $ IdleTimer tm delay

idleTimerSetDelay :: ReactStore IdleTimer -> Int -> IO ()
idleTimerSetDelay store v =
  executeAction $ SomeStoreAction store (SetDelay v)

idleTimerStop :: ReactStore IdleTimer -> IO ()
idleTimerStop store =
  executeAction $ idleTimerStopAction store

idleTimerStart :: ReactStore IdleTimer -> [SomeStoreAction] -> IO ()
idleTimerStart store actions =
  executeAction $ idleTimerStartActions store actions

idleTimerStartActions :: ReactStore IdleTimer -> [SomeStoreAction]
                     -> SomeStoreAction
idleTimerStartActions store actions =
  SomeStoreAction store $ StartTimer actions

idleTimerStopAction :: ReactStore IdleTimer -> SomeStoreAction
idleTimerStopAction store =
  SomeStoreAction store StopTimer
