{-# LANGUAGE OverloadedStrings #-}
-- |

module VK.App.Widgets.AudioPlayer (audioPlayer_
                                  , audioPlayer
                                  , setSongs
                                  , Song(..)
                                  ) where

import           React.Flux

import           VK.App.Widgets.AudioPlayer.Store
import           VK.App.Widgets.AudioPlayer.Types
import qualified VK.App.Widgets.AudioPlayer.Views as V


audioPlayer_ :: ReactElementM eventHandler ()
audioPlayer_ =
  view audioPlayer () mempty

audioPlayer :: ReactView ()
audioPlayer =
  defineControllerView "AudioPlayer" store (\st () -> V.audioPlayer_ st)

