{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |

module VK.App.Widgets.AudioPlayer.Views where

import           Control.Error.Util                 (bool)
import           Data.Aeson                         ((.=))
import qualified Data.Aeson                         as A
import           Data.Fraction
import           Data.Maybe                         (fromMaybe, isNothing)
import           Data.Monoid                        ((<>))
import qualified Data.Text                          as T

#ifdef __GHCJS__
import           GHCJS.Types
#else
import VK.App.Internal.TypePlugs
#endif

import           React.Flux
import           React.Flux.Addons.Bootstrap


import           VK.App.Internal.Utils
import           VK.App.Widgets.AudioPlayer.Actions
import           VK.App.Widgets.AudioPlayer.Store
import           VK.App.Widgets.AudioPlayer.Types


audioPlayer_ :: State ->  ReactElementM eventHandler ()
audioPlayer_ state =
  view audioPlayerView state mempty

audioPlayerView :: ReactView State
audioPlayerView = defineView "AudioPlayer" hlr
  where
    hlr :: State -> ReactElementM ViewEventHandler ()
    hlr st@State{..} =
      div_ ["className" $= "audio-player"] $ do
        unmount_
        div_ ["className" $= "clearfix"] topComponents
        div_ ["className" $= "audio-desc-container clearfix"] $ do
          nameLabel_ songName
          timeLabel_ st
      where
        fraction = maybe (fromFactor (0::Int))
                   (\d ->
                     fromNumber (0, maximum [d, playerSeek, 1]) playerSeek)
                   playerDuration
        topComponents =
         buttonPanel_ st <>
         progressBar_ st (length playerSongs > 1) fraction <>
         volumeBar_ st <>
         if length playerSongs <= 1
         then
           mempty
         else
           songList_ st
        songName
          | length playerSongs <= 1 =
            currentSongName
          | otherwise =
            T.concat [T.pack $ show (fromMaybe 0 currentSongIndex + 1)
                     , ". "
                     , currentSongName]
        currentSongName
          | isNothing currentSongIndex = ""
          | otherwise =
            maybe "" (getSongName . (playerSongs !!)) currentSongIndex

unmount_ :: ReactElementM eventHandler ()
unmount_ = willUnmountView_ "UnmountAudioplayer" onUnmount
  where
    onUnmount = do
      putStrLn "unmount"
      execPlayerAction Unmounted

buttonPanel_ :: State ->  ReactElementM eventHandler ()
buttonPanel_ state =
  view buttonPanelView state mempty

buttonPanelView :: ReactView State
buttonPanelView = defineView "ButtonPanel" hlr
  where
    hlr st@State{..} =
      if length playerSongs < 2
      then
        bootstrap_ "ButtonGroup" ["className" $= buttonPanelClasses] $
        bootstrap_ "Button" ["bsSize" $= "small"
                            , onClick buttonClickHandler] $
        bootstrap_ "Glyphicon" (iconClasses ++ ["glyph" $= iconName]) mempty
      else
        bootstrap_ "ButtonGroup" ["className" $= buttonPanelClasses] $ do
          bootstrap_ "Button" ["bsSize" $= "small"
                              , onClick (\_ _ -> dispatch st Prev)] $
            bootstrap_ "Glyphicon" ["glyph" $= "step-backward"] mempty
          bootstrap_ "Button" ["bsSize" $= "small"
                              , onClick buttonClickHandler] $
            bootstrap_ "Glyphicon" (nextButtonClass ++ ["glyph" $= iconName]) mempty
          bootstrap_ "Button" (nextButtonClass ++
                               ["bsSize" $= "small"
                               , onClick (\_ _ -> dispatch st Next)]) $
            bootstrap_ "Glyphicon" ["glyph" $= "step-forward"] mempty
      where
        buttonClickHandler _ _ =
          dispatch st $ bool Pause Play isShowPlayBtn
        isShowPlayBtn = not isPlaying || isPause
        (iconName, iconClasses) =
          bool (bool "pause" "play" isShowPlayBtn, [])
          ("refresh", ["className" $= "audio-refresh-animate"])
          isLoading
        buttonPanelClasses = "audio-button-panel pull-left"
        nextButtonClass = ["className" $= "disabled" |
                           currentSongIndex == (Just $ length playerSongs - 1)]

nameLabel_ :: T.Text ->  ReactElementM eventHandler ()
nameLabel_ name =
  view nameLabelView name mempty

nameLabelView :: ReactView T.Text
nameLabelView = defineView "NameLabel" $ \name ->
  span_ ["className" $= "audio-name-label pull-left"] $ txt_ name

progressBar_ :: State -> Bool -> Fraction
                -> ReactElementM eventHandler ()
progressBar_ state shorter progress =
  view progressBarView (state, shorter, progress) mempty

progressBarView :: ReactView (State, Bool, Fraction)
progressBarView = defineView "ProgressBar" $ \(st@State{..}, shorter, fraction) ->
  let percent = truncate $ toPercentage fraction :: Int
      style = A.object ["width" .=  T.pack (show percent ++ "%")]
      classes = T.append "audio-progress-container pull-left" $
                if shorter
                then " audio-progress-container-short-width"
                else ""
  in
   div_ ["className" $= classes
        , "style" @= A.object ["marginLeft" .= ("5px"::T.Text)]
        , onClick $ onClickHlr st percent
        ] $
   div_ ["className" $= "audio-progress", "style" @= style] mempty
  where
    onClickHlr st percent evt mevt
      | percent <= 0 = []
      | otherwise =
        let ect = evtCurrentTarget evt
            cx = fromIntegral (mouseClientX mevt)
            (x, _) = elementPos ect
            w = eventTargetInt ect "offsetWidth"
            seek = case fromIntegral (cx - x) / fromIntegral w :: Double of
              v | v >= 1 -> 1
              v -> v
        in
         dispatch st (SeekTo $ fromFactor seek)

volumeBar_ :: State ->  ReactElementM eventHandler ()
volumeBar_ state =
  view volumeBarView state mempty

volumeBarView :: ReactView State
volumeBarView = defineView "VolumeBar" hlr
  where
    hlr :: State -> ReactElementM ViewEventHandler ()
    hlr st@State{..} =
      div_ (("className" $= "audio-volume-bar-container"):mouseLeaveHlr) $ do
        bootstrap_ "Button" ["bsSize" $= "small"
                            , onClick toggleHlr
                            ] $
          bootstrap_ "Glyphicon" ["glyph" $= toggleIcon] mempty
        div_ ["className" $= classes] $ do
          div_ ["className" $= "audio-volume-min-max"
               , onClick (\_ _ ->
                           dispatch st (AdjustVolume $ fromFactor (1::Int)))] $
            bootstrap_ "Glyphicon" ["glyph" $= "volume-up"] mempty
          div_ ["className" $= "audio-volume-percent-container"
               , onClick adjustVolumeToHlr] $
            div_ ["className" $= "audio-volume-percent"
                 , "style" @= style] mempty
          div_ ["className" $= "audio-volume-min-max"
               , onClick (\_ _ ->
                           dispatch st (AdjustVolume $ fromFactor (0::Int)))] $
            bootstrap_ "Glyphicon" ["glyph" $= "volume-off"] mempty
      where
        persent = 100 - (truncate $ toPercentage playerVolume :: Int)
        style = A.object ["top" .= (show persent ++ "%")]
        toggleIcon = bool "volume-up" "volume-off" (toFactor playerVolume <= 0)
        classes = T.append "audio-volume-bar" $
                  bool " audio-volume-bar-hide" "" isVolumeBarVisible
        mouseLeaveHlr =
          [onMouseLeave (\_ _ ->
                          dispatch st HideVolumeBar) |
           isVolumeBarVisible]
        adjustVolumeToHlr evt mevt =
          let ect = evtCurrentTarget evt
              (_, y) = elementPos ect
              cy = mouseClientY mevt
              delta =  cy - y
              h = eventTargetInt ect "offsetHeight"
              frac = fromNumber (0, h) (h - delta)
          in
           dispatch st (AdjustVolume frac)
        toggleHlr _ _
          | isTogglePressed volumeBar =
            dispatch st HideVolumeBar
          | isVolumeBarVisible =
              []
          | otherwise =
                dispatch st ShowVolumeBar

timeLabel_ :: State ->  ReactElementM eventHandler ()
timeLabel_ state =
  view timeLabelView state mempty

timeLabelView :: ReactView State
timeLabelView = defineView "TimeLabel" $ \(State{..}) ->
  let mv = (\d -> T.concat [formatDuration playerSeek, " / ", formatDuration d])
           <$> playerDuration
  in
    case mv of
    Just v -> span_ ["className" $= "audio-time pull-right"] $ txt_ v
    Nothing -> span_ mempty

songList_ :: State ->  ReactElementM eventHandler ()
songList_ state =
  view songListView state mempty

songListView :: ReactView State

songListView = defineView "SongList" $ \(st@State{..}) ->
  div_ ["className" $= "audio-songs-list"] $
  bootstrap_ "DropdownButton" ["open" @= isSongListVisible
#ifdef __GHCJS__
                              , callback "onToggle" $ \(isOpen::Bool) ->
                              dispatch st $ bool HideSongList ShowSongList isOpen
#endif
                              ] $
  mapM_  (uncurry $ songItem_ st) $
  zip playerSongs [0..]

songItem_ :: State -> Song -> Int
             -> ReactElementM eventHandler ()
songItem_ state song idx =
  viewWithKey songItemView idx (state, song, idx) mempty

songItemView :: ReactView (State, Song, Int)
songItemView = defineView "SongItem" $ \(st@State{..}, song, idx) ->
  let isSelected = currentSongIndex == Just idx
      sn = if length playerSongs > 1 then
             T.concat [T.pack $ show (idx + 1), " ", getSongName song]
           else
             getSongName song
      classes = T.append "audio-song-item" $ if isSelected then " active" else ""
      playEl =
        if isSelected && isPlaying
        then
          bootstrap_ "Glyphicon" ["className" $= "audio-song-item-icon active"
                                  , "glyph" $= "play"] mempty
        else
          span_ ["className" $= "audio-song-item-not-selected"] mempty
      songEl =
        span_ ["className" $= "audio-song-item-label"] $ txt_ sn
  in
   bootstrap_ "MenuItem" ["className" $= classes
                         , "eventKey" @= idx
#ifdef __GHCJS__
                         , callback "onSelect" $ \(_::Int) ->
                         dispatch st $ SongItemClick idx
#endif
                         ] (playEl <> songEl)

getSongName :: Song -> T.Text
getSongName song =
  fromMaybe (snd $ T.breakOnEnd "/" $ songUrl song) $ songName song

elementPos :: EventTarget -> (Int, Int)
elementPos el =
  calcPos el (0, 0)
  where
    calcPos et@(EventTarget n) (x, y)
      | isNull n = (x, y)
      | otherwise =
        let net = eventTargetJSVal et "offsetParent"
            [ol, sl, cl] = map (eventTargetInt et)
                           ["offsetLeft", "scrollLeft", "clientLeft"]
            [ot, st, ct] = map (eventTargetInt et)
                           ["offsetTop", "scrollTop", "clientTop"]
        in
         calcPos (EventTarget net) (x + ol + cl - sl, y + ot +ct - st)

eventTargetInt :: EventTarget -> String -> Int
#ifdef __GHCJS__
eventTargetInt et p = eventTargetProp et p
#else
eventTargetInt _ _ = 0
#endif

eventTargetJSVal :: EventTarget -> String -> JSVal
#ifdef __GHCJS__
eventTargetJSVal et p = eventTargetProp et p
#else
eventTargetJSVal _ _ = ()
#endif
