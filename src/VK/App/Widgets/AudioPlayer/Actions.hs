{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
-- |

module VK.App.Widgets.AudioPlayer.Actions where

import           Control.DeepSeq
import           Data.Fraction
import           Data.Typeable                    (Typeable)
import           GHC.Generics                     (Generic)
import qualified Web.Routes.TH                    as WRTH


import           VK.App.Internal.Orphans          ()
import           VK.App.Widgets.AudioPlayer.Types

data Action sd =
  SetSongs ![Song]
  | Unmounted
  | InitSoundObject
  | InitSoundObjectCompleted
  | ClearSoundObject
  | Play
  | PlayContinue
  | PlayEnd
  | Pause
  | Stop
  | Prev
  | Next
  | SeekTo !Fraction
  | ShowVolumeBar
  | HideVolumeBar
  | AdjustVolume !Fraction
  | ShowSongList
  | HideSongList
  | SongItemClick !Int
  | UpdateDuration
  | StopUpdateDuration
  | UpdateSongIndex !Int
  deriving (Show, Typeable, Generic, NFData)

$(WRTH.derivePathInfo ''Action)
