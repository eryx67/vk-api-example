{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
-- |

module VK.App.Widgets.IncrementalInput(incrementalInput
                                      , incrementalInput_
                                      , IncrementalInputArgs(..)
                                      ) where

import qualified Data.Text                         as T
import           Data.Typeable                     (Typeable)

import           React.Flux

import           VK.App.Widgets.Internal.IdleTimer

data IncrementalInputArgs = IncrementalInputArgs {
  iiProperties :: ![PropertyOrHandler (StatefulViewEventHandler T.Text)]
  , iiTimer    :: !(ReactStore IdleTimer)
  , iiOnInput  :: !(T.Text -> [SomeStoreAction])
  }
                          deriving Typeable


incrementalInput :: ReactView IncrementalInputArgs
incrementalInput =
  defineStatefulView "IncrementalInput" "" $
  \curTxt IncrementalInputArgs{..} ->
  input_ $ iiProperties ++
  ["type" $= "text"
  , "value" @= curTxt
  , onChange $ \evt _ ->
  let ctxt = targetValue evt
  in
   if T.length ctxt >= 3 || T.length curTxt > T.length ctxt
   then ([idleTimerStartActions iiTimer $ iiOnInput ctxt], Just ctxt)
   else ([idleTimerStopAction iiTimer], Just ctxt)
  , onBlur $ \_ _ ctxt ->
  ([idleTimerStopAction iiTimer], Just ctxt)
  , onKeyDown $ \_ evt ctxt ->
  if keyCode evt == 13 && not (T.null ctxt)
  then ((idleTimerStopAction iiTimer):iiOnInput ctxt, Just "")
  else ([], Just ctxt)
  ]

incrementalInput_ :: [PropertyOrHandler (StatefulViewEventHandler T.Text)]
                  -> (T.Text -> [SomeStoreAction])
                  -> ReactElementM eventHandler ()
incrementalInput_ props inputActionsHlr =
  view incrementalInput args mempty
  where
    args = IncrementalInputArgs props (mkIdleTimer 3000) inputActionsHlr

targetValue :: Event -> T.Text

#ifdef __GHCJS__
targetValue evt = target evt "value"
#else
targetValue _ = ""
#endif
