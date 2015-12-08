{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Store (controller) for VK application

module VK.App.Store where

import           Control.Concurrent (forkIO)
import           Control.Error.Util (hoistMaybe)
import           Control.Monad (void, when, join)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (runMaybeT)
import qualified Data.Default.Generics as D
import           Data.List (find)
import           Data.Maybe (isNothing, fromMaybe)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Network.API.Builder hiding (runAPI)
import           React.Flux

#ifdef __GHCJS__

import qualified JavaScript.Web.Storage as JSWS
import qualified Data.JSString as JSS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Aeson as Aeson

#else

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

#endif

import qualified VK.API as VK
import qualified VK.API.Audio as VKA

import           VK.App.Types
import           VK.App.Actions
import qualified VK.App.AppSettings as Settings
import           VK.App.Models
import           VK.App.Internal.Utils
import qualified VK.App.Widgets.AudioPlayer as AudioPlayer
import           VK.App.Widgets.ErrorMessage as ErrorMessage

data ViewMode = AudiosMode
              | AuthMode
              deriving (Show, Eq, Typeable)


data AudioFilter = AudioSearch VKA.Search
                 | AudioGet VKA.Get
                 | AudioGetPopular VKA.Popular
                 | NoAudioFilter
              deriving (Show, Typeable)

data AudiosState = AudiosState {
  audiosItems :: !(ItemsPager VKA.Audio)
  , audiosSelector :: !AudioSelector
  , audiosFilter :: !AudioFilter
  , audiosCurrentId :: !(Maybe Int)
  }
                deriving (Show, Typeable)

instance D.Default AudiosState where
  def = AudiosState {
    audiosItems = ItemsPager (VK.Items [] 0) 10 0
    , audiosSelector = OwnAudio
    , audiosFilter = NoAudioFilter
    , audiosCurrentId = Nothing
    }

data State = State {
  apiState     :: !(Maybe (VK.VKApp ApiState))
  , viewMode :: !ViewMode
  , authState :: !AuthState
  , lastAction :: !(Maybe VKAction)
  , lastViewMode :: !ViewMode
  , audios :: !AudiosState
  , isAjaxRunning :: !Bool
  }
           deriving (Show, Typeable)

instance D.Default State where
  def = State {
    apiState = Nothing
    , viewMode = AudiosMode
    , authState = AuthInit
    , lastAction = Nothing
    , lastViewMode = AudiosMode
    , audios = D.def
    , isAjaxRunning = False
    }

instance StoreData State where
  type StoreAction State = VKAction
  transform action state@State{..} = do
    -- print (action, state)
    case action of
      AppInit -> do
        execAction (dispatch state) action
        initState
      Authorize action' ->
        startAuth action'
      SetApiState as ->
        return state{apiState = Just as}
      SetAuthState as ->
        doAuth as
      SetAjaxRunning v ->
        return state{isAjaxRunning = v}
      Audios aaction ->
        transformAudios aaction state
      ApiError (APIError VK.AccessTokenExpired) -> do
        wsRemoveAuthToken
        transform AppInit state
      ApiError e@(HTTPError _) -> do
        setErrorMessage . T.pack $ show e
        wsRemoveAuthToken
        return state
      ApiError e -> do
        setErrorMessage . T.pack $ show e
        return state
      Search _ ->
        return state
      Refresh ->
        return state
    where
      initState = do
        let settings =
              VK.createSettings
              Settings.appId
              "nouser" "nopass"
              (Just Settings.appScope)
        mgr <- newManager tlsManagerSettings
        at <- wsGetAuthToken
        let as = VK.VKApp settings () mgr at
        return $ state{apiState = Just as}
      startAuth action' =
        return state{lastAction = Just action'
                    , authState = AuthInit
                    , viewMode = AuthMode
                    }
      doAuth as@AuthLogin =
        return state{authState = as}
      doAuth AuthCompleted =
        case (apiState, authState) of
           (Just as, AuthURL aurl) -> do
             res <- VK.authorizeVKAppFromURL as aurl
             either
               (\e -> do
                   void . forkIO $ apiError e
                   return state{authState = AuthInit})
               (\nas -> do
                   maybe (return ()) (execAction (dispatch state)) lastAction
                   maybe (return ()) wsSetAuthToken $ VK.getAuthToken nas
                   return state{authState = AuthInit
                               , apiState = Just nas
                               , viewMode = lastViewMode
                               }
               ) res
           _ ->
             return state{authState = AuthInit}
      doAuth as =
        return state{authState = as}

dispatch :: State -> VKAction -> [SomeStoreAction]
dispatch _ AppInit =
  map (SomeStoreAction store . Audios) [SetAudioSelector OwnAudio, GetAudio 0]
dispatch State{..} (Search txt)
  | viewMode == AudiosMode =
    map (SomeStoreAction store . Audios) [SetAudioQuery txt, GetAudio 0]
  | otherwise =
      []
dispatch _ a@(Audios (SetAudioSelector _)) =
  map (SomeStoreAction store) [a, Audios $ GetAudio 0]
dispatch _ a =
  [SomeStoreAction store a]

transformAudios :: AudiosAction -> State -> IO State
transformAudios action st@State{..} = do
  let AudiosState{..} = audios
      count = pageSize audiosItems
  case action of
    SetAudioSelector asel ->
      return st{audios = audios{audiosSelector = asel
                               , audiosFilter = audioSelectorToFilter asel st Nothing}}
    GetAudio offset -> do
      let af = audioFilterSetPage count offset audiosFilter
      runAudioFilter af action st offset
      return st
    SetAudioItems o (VK.AR is) -> do
      let naudios = audios{audiosItems = audiosItems{items = is, offset = o}}
      return st{audios = naudios}
    SetAudio o (VK.AR is) -> do
      let nitems = VKA.Items is (length is)
          naudios = audios{audiosItems = audiosItems{items = nitems, offset = o}}
      return st{audios = naudios}
    SetAudioCurrent aid -> do
      let mau = find ((aid ==) . VKA.audioId) $ VKA.items $ items audiosItems
      case mau of
        Just au -> do
          AudioPlayer.setSongs
            [AudioPlayer.Song (Just $ VKA.audioTitle au) (VKA.audioUrl au)]
          return st{audios = audios{audiosCurrentId = Just aid}}
        Nothing ->
          return st
    SetAudioQuery q -> do
      let af = audioSelectorToFilter audiosSelector st $ Just q
          naudios = audios{audiosFilter = af}
      return st{audios = naudios}

audioSelectorToFilter :: AudioSelector -> State -> Maybe T.Text -> AudioFilter
audioSelectorToFilter OwnAudio st mq
  | mq == Nothing || mq == Just "" =
    AudioGet $ D.def{VKA.getOwnerId = VK.UserId <$> getMyId st}
  | otherwise =
      AudioSearch $ D.def{VKA.searchSearchOwn = Just VKA.OwnRecords
                         , VKA.searchAutoComplete = Just 1
                         , VKA.searchQ = fromMaybe "" mq
                         }
audioSelectorToFilter PopularAudio _ _ =
  AudioGetPopular D.def
audioSelectorToFilter AnyAudio _ (Just q) =
  AudioSearch $ D.def{VKA.searchSearchOwn = Just VKA.AnyRecords
                     , VKA.searchAutoComplete = Just 1
                     , VKA.searchQ = q
                     }
audioSelectorToFilter AnyAudio _ Nothing =
  AudioSearch $ D.def{VKA.searchSearchOwn = Just VKA.AnyRecords
                     , VKA.searchAutoComplete = Just 1
                     , VKA.searchQ = ""
                     }

audioFilterSetPage :: Int -> Int -> AudioFilter -> AudioFilter
audioFilterSetPage count offset (AudioGet af) =
  AudioGet af{VKA.getCount = Just count
             , VKA.getOffset = Just offset
             }
audioFilterSetPage count offset (AudioSearch af) =
  AudioSearch af{VKA.searchCount = Just count
                , VKA.searchOffset = Just offset
                }
audioFilterSetPage count offset (AudioGetPopular af) =
  AudioGetPopular af{VKA.popularCount = Just count
                    , VKA.popularOffset = Just offset
                    }
audioFilterSetPage _ _ af@NoAudioFilter =
  af

runAudioFilter :: AudioFilter -> AudiosAction -> State -> Int -> IO ()
runAudioFilter (AudioGet af) a st offset =
  runAPI st (Audios a) (VK.toAPI af) (Audios . SetAudioItems offset)
runAudioFilter (AudioSearch af) a st offset =
  runAPI st (Audios a) (VK.toAPI af) (Audios . SetAudioItems offset)
runAudioFilter (AudioGetPopular af) a st offset =
  runAPI st (Audios a) (VK.toAPI af) (Audios . SetAudio offset)
runAudioFilter NoAudioFilter _ _ _ =
  return ()

getMyId :: State -> Maybe Int
getMyId State{..} =
  join $ fmap VK.getUserId apiState

runAPI :: State -> VKAction -> VK.VKAPI ApiState a -> (a -> VKAction) -> IO ()
runAPI State{..} action apiAction hlr =
  void . forkIO $ do
    res <- runMaybeT $ do
      as <- hoistMaybe apiState
      _ <- hoistMaybe $ if VK.isAuthorized as then Just True else Nothing
      lift $ do
        alterStore store (SetAjaxRunning True)
        (res, nas) <- VK.runVKAPI as apiAction
        alterStore store (SetApiState nas)
        alterStore store (SetAjaxRunning False)
        either apiError handleAction res
    when (isNothing res) $
      alterStore store (Authorize action)
  where
    handleAction v = alterStore store (hlr v)

apiError :: APIError VK.VKError -> IO ()
apiError e =
  alterStore store (ApiError e)

store :: ReactStore State
store = mkStore D.def


#ifdef __GHCJS__

wsGetAuthToken :: IO (Maybe VK.AuthToken)
wsGetAuthToken = do
  mv <- JSWS.getItem "VK.authToken" $ JSWS.localStorage
  return . join $ (Aeson.decode . BSL.pack . JSS.unpack <$> mv)

wsSetAuthToken :: VK.AuthToken -> IO ()
wsSetAuthToken at =
  JSWS.setItem "VK.authToken" (JSS.pack . BSL.unpack $ Aeson.encode at)
  (JSWS.localStorage)

wsRemoveAuthToken :: IO ()
wsRemoveAuthToken =
  JSWS.removeItem "VK.authToken"  $ JSWS.localStorage

#else

wsGetAuthToken :: IO (Maybe VK.AuthToken)
wsGetAuthToken = return Nothing


wsSetAuthToken :: VK.AuthToken -> IO ()
wsSetAuthToken _ = return ()

wsRemoveAuthToken :: IO ()
wsRemoveAuthToken = return ()

#endif
