{-# LANGUAGE OverloadedStrings, RecordWildCards, TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
-- |

module VK.App.Widgets.AudioPlayer.Store where

import           Control.Concurrent
import           Control.Error.Util
import           Control.Monad (void, liftM, unless)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe
import           Data.Fraction
import           Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import           Data.Typeable (Typeable)

#ifdef __GHCJS__
import           Data.JSString.Text
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Types
#endif

import           React.Flux


import           VK.App.Internal.Orphans ()
import           VK.App.Internal.Utils
import           VK.App.Widgets.AudioPlayer.Actions
import VK.App.Widgets.AudioPlayer.Types

import VK.App.Internal.TypePlugs

-- import Debug.Trace

data VolumeBar = VolumeBar {
  isTogglePressed :: !Bool
  }
               deriving (Show, Typeable)

data Howler = Howler {
  howlerRef :: !JSVal
  , howlerInitCb :: !(Maybe (Callback (IO ())))
  , howlerPlayEndCb :: !(Maybe (Callback (IO ())))
  }

instance Show Howler where
  show _ = "Howler"

data State = State {
  isPlaying          :: !Bool
  , isPause          :: !Bool
  , isLoading        :: !Bool
  , isSongListVisible :: !Bool
  , isVolumeBarVisible :: !Bool
  , playerSeek :: !Int
  , playerDuration :: !(Maybe Int)
  , playerVolume           :: !Fraction
  , currentSongIndex :: !(Maybe Int)
  , playerSongs            :: ![Song]
  , volumeBar :: !VolumeBar
  , howler :: !Howler
  , howlerCurrentSrc :: !(Maybe T.Text)
  , updateInterval :: !(Maybe ThreadId)
  }
           deriving (Show, Typeable)

instance StoreData State where
  type StoreAction State = Action State
  transform action st@State{..} = do
    case action of
      SetSongs songs ->
        return st{playerSongs = songs
                 , currentSongIndex = case songs of
                   [] -> Nothing
                   _ -> Just 0
                 }
      InitSoundObject -> do
        mst <- runMaybeT $ do
          lift . void $ clearSoundObject howler
          cidx <- hoistMaybe currentSongIndex
          hwlr <- lift $ newHowler st cidx
          return st{howler = hwlr, isLoading = True}
        return $ fromMaybe st mst
      InitSoundObjectCompleted -> do
        drn <- howlerDuration howler
        hwlrSrc <- howlerSrc howler
        return st{playerDuration = drn, isLoading = False, howlerCurrentSrc = hwlrSrc}
      ClearSoundObject -> do
        hwlr <- clearSoundObject howler
        return st{howler = hwlr,  howlerCurrentSrc = Nothing, playerDuration = Nothing}
      Play ->
        return st{ isPlaying = True, isPause = False}
      PlayContinue -> do
        howlerPlay howler
        ival <- updateCurrentDuration 1000000
        return st{updateInterval = Just ival}
      PlayEnd ->
        return st
      Stop -> do
        return st{playerSeek = 0, isPlaying = False}
      Unmounted ->
        return st
      Pause -> do
        howlerPause howler
        return st{isPause = True}
      UpdateDuration -> do
        seek <- howlerCurrentPos howler
        return st{playerSeek = seek}
      StopUpdateDuration -> do
        stopUpdateCurrentDuration st
      Prev ->
        return st
      Next ->
        return st
      UpdateSongIndex idx ->
        return st{currentSongIndex = Just idx, playerDuration = Just 0}
      SongItemClick _ ->
        return st
      SeekTo pos ->
        seekTo pos
      AdjustVolume vol ->
        adjustVolume vol
      ShowVolumeBar ->
        return st{isVolumeBarVisible = True}
      HideVolumeBar ->
        return st{isVolumeBarVisible = False}
      ShowSongList ->
        return st{isSongListVisible = True}
      HideSongList ->
        return st{isSongListVisible = False}
      where
        seekTo fraction = do
          let seek = maybe 0 (\v ->
                                truncate $ fromIntegral v * toFactor fraction)
                     playerDuration
          howlerSeek howler $ fromIntegral seek
          return st{playerSeek = seek}
        adjustVolume vol = do
          howlerVolume howler $ toFactor vol
          return st{playerVolume = vol}

setSongs :: [Song] -> IO ()
setSongs =
  execPlayerAction . SetSongs

store :: ReactStore State
store = mkStore State {
  isPlaying = False
  , isPause = False
  , isLoading = False
  , isSongListVisible = False
  , isVolumeBarVisible = False
  , playerSeek = 0
  , playerDuration = Nothing
  , playerVolume = fromFactor (0.5::Double)
  , currentSongIndex = Nothing
  , playerSongs = []
  , volumeBar = VolumeBar False
  , howler = Howler nullRef Nothing Nothing
  , howlerCurrentSrc = Nothing
  , updateInterval = Nothing
  }

execPlayerAction :: Action State -> IO ()
execPlayerAction action = do
  st <- getStoreData store
  execAction (dispatch st) action

dispatch :: State -> Action State -> [SomeStoreAction]
dispatch State{..} action =
  map (SomeStoreAction store) $ doRoute action
  where
    doRoute a@(SetSongs _) =
      a:doRoute Unmounted
    doRoute a@Unmounted =
      [Stop, StopUpdateDuration, ClearSoundObject, a]
    doRoute a@InitSoundObjectCompleted
      | isLoading =
        [a, StopUpdateDuration, PlayContinue]
      | otherwise = []
    doRoute _
      | isLoading = []
    doRoute Play
      | isPlaying && not isPause = []
      | isNull $ howlerRef howler =
        [InitSoundObject, Play]
      | isNothing currentSongIndex ||
        (Just . songUrl $ playerSongs !! fromMaybe 0 currentSongIndex) /= howlerCurrentSrc =
        [InitSoundObject, Play]
      | otherwise =
        Play:doRoute PlayContinue
    doRoute Pause
      | isPause = doRoute PlayContinue
      | otherwise = Pause:doRoute StopUpdateDuration
    doRoute a@PlayEnd
      | currentSongIndex == (Just $ length playerSongs - 1) =
        a:doRoute Stop
      | otherwise =
        a:doRoute Next
    doRoute a@Stop =
      a:doRoute StopUpdateDuration
    doRoute a@PlayContinue =
      doRoute StopUpdateDuration ++ [a]
    doRoute Next =
      case currentSongIndex of
      Just cidx ->
        doRoute $ UpdateSongIndex $ cidx + 1
      Nothing ->
        []
    doRoute Prev
      | playerSeek > 5 || currentSongIndex == Just 0 =
        doRoute . SeekTo $ (fromFactor (0::Double))
      | otherwise =
        case currentSongIndex of
        Just cidx ->
          doRoute . UpdateSongIndex $ max 0 (cidx - 1)
        Nothing ->
          []
    doRoute a@(UpdateSongIndex _)
      | isPlaying = a:HideSongList:concatMap doRoute [Stop, ClearSoundObject] ++
                    [InitSoundObject, Play]
      | otherwise = a:HideSongList:concatMap doRoute [Stop, ClearSoundObject]
    doRoute (SongItemClick idx)
      | currentSongIndex == Just idx && isPause =
        HideSongList:doRoute Pause
      | currentSongIndex == Just idx && not isPause =
        HideSongList:doRoute Play
      | otherwise =
        doRoute (UpdateSongIndex idx)
    doRoute a = [a]

clearSoundObject :: Howler -> IO Howler
clearSoundObject howler = do
  howlerStop howler
  destroyHowler howler
  return $ Howler nullRef Nothing Nothing

stopUpdateCurrentDuration :: State -> IO State
stopUpdateCurrentDuration st@State{..} =
  case updateInterval of
  Nothing -> return st
  Just tid -> do
    killThread tid
    return st{updateInterval = Nothing}

updateCurrentDuration :: Int -> IO ThreadId
updateCurrentDuration ival = forkIO doUpdate
  where
    doUpdate = do
      threadDelay ival
      execPlayerAction UpdateDuration
      doUpdate

newHowler :: State -> Int -> IO Howler
newHowler State{..} idx = do
  initCb <- asyncCallback $ execPlayerAction InitSoundObjectCompleted
  playendCb <- asyncCallback $ execPlayerAction PlayEnd
  let Song{..} = playerSongs !! idx
  vol <- toJSVal_aeson $ toFactor playerVolume
  urls <- toJSValListOf $ [textToJSString songUrl]
  autoplay <- toJSVal_aeson False
  args <- createObj
  mapM_ (\(k, v) -> setObjProp k v args)
    [("urls", urls)
    , ("volume", vol)
    , ("autoplay", autoplay)
    , ("onload", jsval initCb)
    , ("onend", jsval playendCb)
    ]
  howlerRef <- js_NewHowl (jsval args)
  return $ Howler howlerRef (Just initCb) (Just playendCb)

destroyHowler :: Howler -> IO ()
destroyHowler Howler{..} = do
  unless (isNull howlerRef) $ do
    maybe (return ()) releaseCallback howlerInitCb
    maybe (return ()) releaseCallback howlerPlayEndCb

howlerDuration :: Howler -> IO (Maybe Int)
howlerDuration Howler{..} =
  if isNull howlerRef
  then return Nothing
  else liftM Just $ js_howlerDuration howlerRef

howlerPlay :: Howler -> IO ()
howlerPlay Howler{..} =
  unless (isNull howlerRef) $
  js_howlerPlay howlerRef

howlerStop :: Howler -> IO ()
howlerStop Howler{..} =
  unless (isNull howlerRef) $
  js_howlerStop howlerRef

howlerPause :: Howler -> IO ()
howlerPause Howler{..} =
  unless (isNull howlerRef) $
  js_howlerPause howlerRef

howlerSeek :: Howler -> Double -> IO ()
howlerSeek Howler{..} v =
  unless (isNull howlerRef) $
  js_howlerSeek howlerRef v

howlerCurrentPos :: Howler -> IO Int
howlerCurrentPos Howler{..} =
  if isNull howlerRef
  then return 0
  else do
    pos <- js_howlerPosition howlerRef
    return . truncate $ pos

howlerVolume :: Howler -> Double -> IO ()
howlerVolume Howler{..} v =
  unless (isNull howlerRef) $
  js_howlerVolume howlerRef v

howlerSrc :: Howler -> IO (Maybe T.Text)
howlerSrc Howler{..} =
  if isNull howlerRef
  then return Nothing
  else do
    jstr <- js_howlerSrc howlerRef
    return . Just $ textFromJSString jstr

#ifdef __GHCJS__

foreign import javascript unsafe
  "(function () {return new Howl($1);})()"
  js_NewHowl :: JSVal -> IO JSVal

foreign import javascript unsafe
  "($1)._duration"
  js_howlerDuration :: JSVal -> IO Int

foreign import javascript unsafe
  "($1).play()"
  js_howlerPlay :: JSVal -> IO ()

foreign import javascript unsafe
  "($1).stop()"
  js_howlerStop :: JSVal -> IO ()

foreign import javascript unsafe
  "($1).pause()"
  js_howlerPause :: JSVal -> IO ()

foreign import javascript unsafe
  "($1).pos($2)"
  js_howlerSeek :: JSVal -> Double -> IO ()

foreign import javascript unsafe
  "($1).pos()"
  js_howlerPosition :: JSVal -> IO Double

foreign import javascript unsafe
  "($1).volume($2)"
  js_howlerVolume :: JSVal -> Double -> IO ()

foreign import javascript unsafe
  "($1)._src"
  js_howlerSrc :: JSVal -> IO JSString

#else

js_NewHowl :: JSVal -> IO JSVal
js_NewHowl _ = return ()

js_howlerDuration :: JSVal -> IO Int
js_howlerDuration _ = return 0

js_howlerPlay :: JSVal -> IO ()
js_howlerPlay _ = return ()

js_howlerStop :: JSVal -> IO ()
js_howlerStop _ = return ()

js_howlerPause :: JSVal -> IO ()
js_howlerPause _ = return ()

js_howlerSeek :: JSVal -> Double -> IO ()
js_howlerSeek _ _ = return ()

js_howlerPosition :: JSVal -> IO Double
js_howlerPosition _ = return 0

js_howlerVolume :: JSVal -> Double -> IO ()
js_howlerVolume _ _ = return ()

js_howlerSrc :: JSVal -> IO JSString
js_howlerSrc _ = return ""

#endif
