{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
-- |

module VK.App.Internal.Orphans where

import           Control.Applicative           ((<|>))
import           Control.DeepSeq
import           Data.Fraction
import           GHC.Generics                  (Generic)
import qualified Web.Routes                    as WR
import           Network.API.Builder           (APIError (..))

#ifdef __GHCJS__
import           JavaScript.Web.XMLHttpRequest (XHRError (..))
import           Network.API.Builder           (Manager (..))
#endif

import qualified VK.API                        as VK
import qualified VK.API.Audio                  as VKA


instance Show a => Show (VK.VKApp a) where
  show VK.VKApp{..} =
    "VKApp " ++ show _vkApp

#ifdef __GHCJS__
deriving instance Generic Manager

deriving instance NFData Manager

deriving instance NFData XHRError

deriving instance Generic (VK.VKApp a)
deriving instance NFData (VK.VKApp ())

deriving instance Generic a => Generic (APIError a)
deriving instance (NFData a, Generic a) => NFData (APIError a)

#else

instance NFData (VK.VKApp ()) where
  rnf _ = error "should never happen"

instance NFData (APIError a) where
  rnf _ = error "should never happen"

#endif


deriving instance Generic VK.VKSettings
deriving instance NFData  VK.VKSettings

deriving instance Generic VK.AuthUser
deriving instance NFData  VK.AuthUser

deriving instance NFData  VK.AuthToken

deriving instance Generic VK.AuthArgs
deriving instance NFData  VK.AuthArgs

deriving instance Generic VK.AuthResponseType
deriving instance NFData  VK.AuthResponseType

deriving instance Generic VK.AuthPermissions
deriving instance NFData  VK.AuthPermissions

deriving instance Generic VK.AuthDisplay
deriving instance NFData  VK.AuthDisplay

instance WR.PathInfo (VK.VKApp a) where
   toPathSegments _ = error "shouldn't happen"
   fromPathSegments = error "shouldn't happen"

deriving instance NFData a => NFData (VK.Items a)

deriving instance NFData a => NFData (VK.ActionResponse a)

instance WR.PathInfo (VK.ActionResponse a) where
   toPathSegments _ = error "shouldn't happen"
   fromPathSegments = error "shouldn't happen"

deriving instance NFData VKA.AudioGenre

deriving instance NFData VKA.AudioUser

deriving instance Generic VKA.OwnerId
deriving instance NFData VKA.OwnerId

deriving instance NFData VKA.Audio

instance WR.PathInfo Bool

instance NFData Fraction where
  rnf !_ = ()

instance WR.PathInfo Double

instance Show Fraction where
  show v = "Fraction " ++ (show $ toFactor v)

instance WR.PathInfo Fraction where
  toPathSegments v = WR.toPathSegments $ toFactor v
  fromPathSegments = fromFactor <$> (WR.fromPathSegments :: WR.URLParser Double)

deriving instance Generic VK.VKError
deriving instance NFData VK.VKError

instance WR.PathInfo (APIError a) where
   toPathSegments _ = error "shouldn't happen"
   fromPathSegments = error "shouldn't happen"

instance WR.PathInfo a => WR.PathInfo (Maybe a) where
  toPathSegments (Just v) = "just":WR.toPathSegments v
  toPathSegments Nothing = ["nothing"]
  fromPathSegments = (WR.segment "nothing" *> pure Nothing)
                     <|> (WR.segment "just" *> (Just <$> WR.fromPathSegments))
