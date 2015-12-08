{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, TypeFamilies, RankNTypes #-}

#ifdef __GHCJS__
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
#endif
-- |

module VK.DOM.Router (getLocation
                     , setLocation
                     , setLocationHash
                     , getLocationHash
                     , onLocationHashChange
                     , actionRoute
                     , childRoutePath
                     , initRouter
                     , storeRouter
                     ) where

import           React.Flux

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import           Control.Monad.IO.Class (liftIO)
import qualified Web.Routes as WR
import           Data.Foldable (forM_)


#ifdef __GHCJS__

import           Control.Monad (liftM)
import qualified Data.JSString as JSS
import           GHCJS.Foreign.Callback
import           GHCJS.Types (JSString, JSVal)
import           Unsafe.Coerce

#endif

#ifdef __GHCJS__

foreign import javascript unsafe
  "window.onhashchange = function() {$1(location.hash.toString());}"
  js_attachtLocationHashCb :: (Callback (JSVal -> IO ())) -> IO ()

foreign import javascript unsafe
  "window.location.hash = $1"
  js_setLocationHash :: JSString -> IO ()

foreign import javascript unsafe
  "window.location.hash.toString()"
  js_getLocationHash :: IO JSString

foreign import javascript unsafe
  "window.location.replace($1)"
  js_setLocation :: JSString -> IO ()

foreign import javascript unsafe
  "window.location.toString()"
  js_getLocation :: IO JSString

setLocation :: String -> IO ()
setLocation = js_setLocation . JSS.pack

getLocation :: IO (Maybe String)
getLocation = do
  rt <- liftM JSS.unpack js_getLocation
  return $ case rt of
    "" -> Nothing
    _ -> Just rt

setLocationHash :: String -> IO ()
setLocationHash = js_setLocationHash . JSS.pack

getLocationHash :: IO (Maybe String)
getLocationHash = do
  rt <- liftM JSS.unpack js_getLocationHash
  return $ case rt of
    "" -> Nothing
    _ -> Just rt

onLocationHashChange :: (String -> IO ()) -> IO ()
onLocationHashChange fn = do
  cb <- syncCallback1 ThrowWouldBlock (fn . JSS.unpack . unsafeCoerce)
  js_attachtLocationHashCb cb

# else
getLocation :: IO (Maybe String)
getLocation = return Nothing

setLocation :: String -> IO ()
setLocation _ = return ()

setLocationHash :: String -> IO ()
setLocationHash _ = return ()

getLocationHash :: IO (Maybe String)
getLocationHash = return Nothing

onLocationHashChange :: (String -> IO ()) -> IO ()
onLocationHashChange _ = return ()

#endif

childRoutePath :: WR.PathInfo action => action -> [T.Text]
childRoutePath = WR.toPathSegments

actionRoute :: (StoreData store, WR.PathInfo (StoreAction store)) =>
               store -> Maybe ([T.Text] -> T.Text) -> StoreAction store -> T.Text
actionRoute _ mparentRouter action =
  frag
  where
    path = maybe (WR.toPathInfo action) ($ childRoutePath action) mparentRouter
    frag = if "#" `T.isPrefixOf` path
           then path
           else T.cons '#' path

initRouter :: ([T.Text] -> IO ()) -> IO ()
initRouter router =
  onLocationHashChange $ router . stripHash . WR.decodePathInfo . BC.pack
  where
    stripHash ("#":path) = path
    stripHash path = path

storeRouter ::
  WR.PathInfo action =>
  (action -> [SomeStoreAction]) -> ([T.Text] -> IO ())
storeRouter dispatch =
  let site = WR.mkSitePI $ WR.runRouteT $ routerExec
  in
    \rt -> either (const $ return ()) id $ WR.runSite "" site rt
  where
    routerExec action =
      liftIO $ forM_ (dispatch action) executeAction
