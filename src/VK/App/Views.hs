{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
-- | The views for VK application
module VK.App.Views (appView_) where

import           Data.Aeson                  ((.=))
import qualified Data.Aeson                  as A
import qualified Data.Default.Generics       as D
import           Data.Foldable               (forM_)
import qualified Data.Text                   as T
import           React.Flux
import           React.Flux.Addons.Bootstrap


import qualified VK.API                      as VK
import qualified VK.API.Audio                as VKA
import qualified VK.API.Auth                 as Auth
import           VK.App.Actions
import qualified VK.App.AppSettings          as Settings
import           VK.App.Internal.Utils
import           VK.App.Store
import           VK.App.Types                hiding (App (..))
import           VK.App.Widgets
import           VK.DOM.Router

-- import           Debug.Trace

appView :: ReactView (State, ParentRouter)
appView = defineView "app" $ \(st@State{..}, pr) ->
  div_ $ do
    loadSpinner_ isAjaxRunning
    navbarView_ st pr
    div_ ["className" $= "container container-fluid"] $
      div_ ["className" $= "row"] $
        case viewMode of
          AuthMode ->
            div_ ["className" $= "col-lg-12 text-center"] $ do
              errorMessage_ []
              authorizationView_ st pr
          AudiosMode -> do
            div_ ["className" $= "col-sm-3 col-md-2 sidebar"] $
              audioSelectorsView_ st pr
            div_ ["className" $= "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main"] $ do
              errorMessage_ []
              defaultView_ st pr
{-# NOINLINE appView #-}

appView_ :: State -> ParentRouter -> ReactElementM eventHandler ()
appView_ st pr =
  view appView (st, pr) mempty

loadSpinnerView :: ReactView Bool
loadSpinnerView =
  defineView "spinner" $ \case
    True ->
      bootstrap_ "Modal" [callback "onHide" loaderOnHide
                         , "show" @= True
                         , "keyboard" @= False
                         , "backdrop" @= False
                         , "dialogClassName" $= "spinner-loader"
                         ] $
      bootstrap_ "ModalBody" [] $
      span_ ["className" $= "throbber-loader"] $ txt_ "Loading"
    False ->
      mempty
  where
    loaderOnHide :: ViewEventHandler
    loaderOnHide = []
{-# NOINLINE loadSpinnerView #-}

loadSpinner_ :: Bool -> ReactElementM eventHandler ()
loadSpinner_ mustShow =
  view loadSpinnerView mustShow mempty

navbarView :: ReactView (State, ParentRouter)
navbarView = defineView "app" $ \(st@State{..}, _) ->
  case viewMode of
    AuthMode ->
      div_ mempty
    AudiosMode ->
      bootstrap_ "Navbar" ["inverse" @= True
                          , "fixedTop" @= True
                          , "fluid" @= True] $ do
        div_ ["className" $= "navbar-header"] $
          a_ ["className" $= "navbar-brand"
             , "href" $= "#"] $ txt_ "VK"
        bootstrap_ "CollapsibleNav" [] $
          form_ ["className" $= "navbar-form navbar-right"] $
          searchInput_ st
{-# NOINLINE navbarView #-}

navbarView_ :: State -> ParentRouter -> ReactElementM eventHandler ()
navbarView_ st pr =
  view navbarView (st, pr) mempty

searchInput_ :: State -> ReactElementM eventHandler ()
searchInput_ st@State{..} =
  incrementalInput_ ["className" $= "form-control"
                    , "disabled" @= (audiosSelector audios == PopularAudio)
                    , "placeholder" $= "Search..."
                    , "type" $= "text"] $
  \txt -> dispatch st $ Search txt

audioSelectorsView :: ReactView (State, ParentRouter)
audioSelectorsView = defineView "audioSelectors" $ \(st@State{..}, pr) ->
  let asels = zip [1..] [minBound ..]::[(Int, AudioSelector)]
  in
   ul_ ["className" $= "nav nav-sidebar"] $
    forM_ asels
    (\(k, asel) ->
      viewWithKey audioSelectorListView k (st, pr, asel) mempty)
{-# NOINLINE audioSelectorsView #-}

audioSelectorsView_ :: State -> ParentRouter -> ReactElementM eventHandler ()
audioSelectorsView_ st pr =
  view audioSelectorsView (st, pr) mempty

audioSelectorListView :: ReactView (State, ParentRouter, AudioSelector)
audioSelectorListView = defineView "audioSelectorList"  $ \(st@State{..}, pr, asel) ->
  let AudiosState{..} = audios
      lbl = elemShow asel
  in
   if audiosSelector == asel
   then li_ ["className" $= "active"] $
        a_ ["href" $= "#"] lbl
   else li_ [] $
        a_ ["href" $= actionRoute st pr (Audios $ SetAudioSelector asel)] lbl
{-# NOINLINE audioSelectorListView #-}

defaultView :: ReactView (State, ParentRouter)
defaultView =
  defineView "default" $  \(st, pr) ->
  div_ ["className" $= "default"] $
  audiosView_ st pr
{-# NOINLINE defaultView #-}

defaultView_ :: State -> ParentRouter -> ReactElementM eventHandler ()
defaultView_ st pr =
  view defaultView (st, pr) mempty

authorizationView :: ReactView (State, ParentRouter)
authorizationView =
  defineView "authorization" $  \(st@State{..}, _) -> do
    let (loginClass, continueClass, inputAttrs) =
          case authState of
            AuthInit -> (id, disabledClass, ["disabled" $= "true"])
            AuthLogin -> (disabledClass, id, [])
            AuthURL _ -> (disabledClass, id, [])
            AuthCompleted -> (disabledClass, disabledClass, ["disabled" $= "true"])
    div_ ["className" $= "authorization panel panel-default"] $ do
      div_ ["className" $= "panel-heading"] $ do
        p_ [] $ txt_ "Please press button to open a VKontakte login page."
        p_ [] $ txt_ "Insert resulting url here"
      div_ ["className" $= "panel-body"] $ do
        a_ ["className" $= loginClass "btn btn-default"
           , "href" $= authURL
           , "target" $= "_blank"
           , onClick (\_ _ ->
                       dispatch st $ SetAuthState AuthLogin)
           ] $ txt_ "Login"
        form_ ["className" $= "form-horizontal"] $ do
          div_ ["className" $= "form-group"] $
            input_ $ ["className" $= continueClass "form-control"
                     , "type" $= "text"
                     , "placeholder" $= "Authorization URL"
                     , onChange (\evt ->
                                  let val = targetValue evt
                                  in
                                   dispatch st $ SetAuthState (AuthURL val))
                     ] ++ inputAttrs
          button_ ["className" $= continueClass "btn btn-default", "type" $= "submit"
                  , onClick (\evt _ ->
                              preventDefault evt : dispatch st (SetAuthState AuthCompleted))
                  ] $ txt_ "Continue"
  where
    disabledClass = T.append "disabled "
    authURL = Auth.oauthURL Settings.appId D.def{VK.authScope = Just Settings.appScope}

authorizationView_ :: State -> ParentRouter -> ReactElementM eventHandler ()
authorizationView_ st pr =
  view authorizationView (st, pr) mempty

audiosView :: ReactView (State, ParentRouter)
audiosView = defineView "audios" $ \(st@State{..}, pr) ->
  let AudiosState{..} = audios
      pgWidget = audiosList st pr audiosCurrentId
      pgRoute = actionRoute st pr . Audios . GetAudio
  in
    pagerView audiosItems 10 pgWidget pgRoute
  where
    audiosList :: State -> ParentRouter -> Maybe Int -> [VKA.Audio]
                  -> ReactElementM ViewEventHandler ()
    audiosList st pr cid audios =
      div_ ["className" $= "row audio-list"
           , "style" @= A.object ["display" .= ("flex"::String)
                                 , "flexWrap" .= ("wrap"::String)]
           ] $
      ul_ ["className" $= "list-group"] $
      forM_ audios (audioListView_ st pr cid)
{-# NOINLINE audiosView #-}

audiosView_ :: State -> ParentRouter -> ReactElementM eventHandler ()
audiosView_ st pr =
  view audiosView (st, pr) mempty

audioListView :: ReactView (State, ParentRouter, Maybe Int, VKA.Audio)
audioListView =
  defineView "audioListView" audioRender
  where
    audioRender :: (State, ParentRouter, Maybe Int, VKA.Audio) -> ReactElementM ViewEventHandler ()
    audioRender (st, _, cid, a)
      | cid == (Just $ VKA.audioId a) =
        li_ ["className" $= "list-group-item audio-item"]
        audioPlayer_
      | otherwise =
        li_ ["className" $= "list-group-item audio-item"] $ do
          button_ ["className" $= "btn glyphicon glyphicon-play-circle"
                   , onClick (\_ _ ->
                               dispatch st (Audios . SetAudioCurrent $ VKA.audioId a))
                  ] mempty
          span_ $ txt_ $ T.concat [VKA.audioTitle a, "/", VKA.audioArtist a]
          span_ $ txt_ . formatDuration $ VKA.audioDuration a
{-# NOINLINE audioListView #-}

audioListView_ :: State -> ParentRouter -> Maybe Int -> VKA.Audio
              -> ReactElementM eventHandler ()
audioListView_ st pr cid a =
  viewWithKey audioListView (VKA.audioId a) (st, pr, cid, a) mempty

targetValue :: Event -> T.Text

#ifdef __GHCJS__
targetValue evt = target evt "value"
#else
targetValue _ = ""
#endif
