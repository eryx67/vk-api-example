{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
-- |

module VK.App.Widgets.ErrorMessage(errorMessage
                                  , errorMessage_
                                  , setErrorMessage
                                  , setShowTime
                                  ) where

import           Control.DeepSeq
import qualified Data.Text                         as T
import           Data.Typeable                     (Typeable)
import           GHC.Generics                      (Generic)


import           React.Flux
import           React.Flux.Addons.Bootstrap


import           VK.App.Internal.Utils
import           VK.App.Widgets.Internal.IdleTimer

data Action =
  SetMessage !T.Text
  | ClearMessages
  | SetTimerDelay !Int
  deriving (Typeable, Generic, NFData)

data State = State {
  emTimer     :: !(ReactStore IdleTimer)
  , emMessage :: !(Maybe T.Text)
  }
           deriving Typeable

instance StoreData State where
  type StoreAction State = Action

  transform action st@State{..} = do
    case action of
      SetTimerDelay showTime -> do
        idleTimerSetDelay emTimer showTime
        return st
      SetMessage msg -> do
        idleTimerStart emTimer [SomeStoreAction store ClearMessages]
        return st{emMessage = Just msg}
      ClearMessages -> do
        idleTimerStop emTimer
        return st{emMessage = Nothing}

store :: ReactStore State
store = mkStore $ State (mkIdleTimer 3000) Nothing
{-# NOINLINE store #-}

setErrorMessage :: T.Text -> IO ()
setErrorMessage msg =
  alterStore store $ SetMessage msg

setShowTime :: Int -> IO ()
setShowTime tm =
  alterStore store $ SetTimerDelay tm

errorMessage :: ReactView [PropertyOrHandler ViewEventHandler]
errorMessage =
  defineControllerView "ErrorMessage" store $ \State{..} props ->
  case emMessage of
    Nothing ->
      mempty
    Just msg ->
      let props1 = ["bsStyle" $= "danger"
                   , callback "onDismiss" onDismiss
                   ] ++ props
      in
       bootstrap_ "Alert" props1 $ do
         p_ $ txt_ msg
         bootstrap_ "Button" [callback "onClick" onDismiss] $ txt_ "Close"
  where
    onDismiss :: ViewEventHandler
    onDismiss = [SomeStoreAction store ClearMessages]
{-# NOINLINE errorMessage #-}

errorMessage_ :: [PropertyOrHandler ViewEventHandler]
                 -> ReactElementM eventHandler ()
errorMessage_ props =
  view errorMessage props mempty

