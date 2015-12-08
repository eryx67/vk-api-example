{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |

module VK.App.Widgets.AudioPlayer.Types where

import           Control.DeepSeq
import qualified Data.Text                          as T
import           Data.Typeable                      (Typeable)
import           GHC.Generics                       (Generic)
import           Text.ParserCombinators.Parsec.Prim (many)
import qualified Web.Routes                         as WR
import qualified Web.Routes.TH                      as WRTH

import           VK.App.Internal.Orphans            ()


data Song = Song {
  songName  :: !(Maybe T.Text)
  , songUrl :: !T.Text
  }
          deriving (Show, Typeable, Generic, NFData)

$(WRTH.derivePathInfo ''Song)


instance WR.PathInfo [Song] where
  toPathSegments vs = "songs":concatMap WR.toPathSegments vs
  fromPathSegments = WR.segment "songs" *> many WR.fromPathSegments

