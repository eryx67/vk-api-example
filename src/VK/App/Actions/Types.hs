{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
-- | Data types for application actions.

module VK.App.Actions.Types where

import           Control.DeepSeq
import qualified Data.Text               as T
import           Data.Typeable           (Typeable)
import           GHC.Generics            (Generic)
import qualified Web.Routes.TH           as WRTH


import qualified VK.API                  as VK
import qualified VK.API.Audio            as VKA

import           VK.App.Internal.Orphans ()

data AuthState = AuthInit
               | AuthLogin
               | AuthURL T.Text
               | AuthCompleted
              deriving (Show, NFData, Generic, Typeable)

$(WRTH.derivePathInfo ''AuthState)

data AudioSelector = OwnAudio
                   | AnyAudio
                   | PopularAudio
                   deriving (Show, Typeable, Generic, Eq, Enum, Bounded, NFData)

$(WRTH.derivePathInfo ''AudioSelector)

data AudiosAction = GetAudio !Int
                    -- ^ get user own audio from offset
                  | SetAudioItems !Int (VK.ActionResponse (VK.Items VKA.Audio))
                  | SetAudio !Int (VK.ActionResponse [VKA.Audio])
                    -- ^ set audio records and offset
                  | SetAudioCurrent !Int
                  | SetAudioSelector !AudioSelector
                  | SetAudioQuery !T.Text
                  deriving (Show, Typeable, Generic, NFData)

$(WRTH.derivePathInfo ''AudiosAction)
