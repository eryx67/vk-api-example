-- | 

module VK.App.Internal.TypePlugs where

#ifdef __GHCJS__

import           GHCJS.Types
import qualified JavaScript.Object as JO

createObj :: IO JO.Object
createObj = JO.create

setObjProp :: JSString -> JSVal -> JO.Object -> IO ()
setObjProp = JO.setProp

#else

import           Data.Aeson hiding (Object)
import           Data.Text (Text, pack, unpack)

type Callback a = ()
type JSVal = ()
type JSString = String
type JSArray = JSVal
type Object = JSVal

class ToJSVal a
instance ToJSVal Value
instance ToJSVal Text
instance ToJSVal ()

createObj :: IO Object
createObj = return ()

setObjProp :: JSString -> JSVal -> Object -> IO ()
setObjProp _ _ _ = return ()

nullRef :: JSVal
nullRef = ()

isNull :: JSVal -> Bool
isNull _ = True

asyncCallback :: IO ()	-> IO (Callback (IO ()))
asyncCallback _ = return ()

toJSVal_aeson :: ToJSON a => a -> IO JSVal
toJSVal_aeson _ = return ()

toJSValListOf :: [a] -> IO JSVal
toJSValListOf _ = return ()

textToJSString :: Text -> JSString
textToJSString = unpack

textFromJSString :: JSString -> Text
textFromJSString = pack

jsval :: a -> JSVal
jsval _ = ()

releaseCallback :: Callback a -> IO ()
releaseCallback _ = return ()
#endif
