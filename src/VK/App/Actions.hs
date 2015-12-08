{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
-- |

module VK.App.Actions (VKAction(..)
                      , module Exp) where

import           Control.DeepSeq
import qualified Data.Text               as T
import           Data.Typeable           (Typeable)
import           GHC.Generics            (Generic)
import           Network.API.Builder     (APIError (..))
import qualified Web.Routes.TH           as WRTH


import qualified VK.API                  as VK
import           VK.App.Actions.Types    as Exp
import           VK.App.Internal.Orphans ()
import           VK.App.Types

data VKAction = AppInit
              | SetAjaxRunning !Bool
              | SetApiState !(VK.VKApp ApiState)
              | Authorize !VKAction
              | SetAuthState !AuthState
              | Audios !AudiosAction
              | Search !T.Text
              | ApiError !(APIError VK.VKError)
              | Refresh
              deriving (Show, Typeable, Generic, NFData)

$(WRTH.derivePathInfo ''VKAction)
