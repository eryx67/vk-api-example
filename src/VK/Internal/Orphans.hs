{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module VK.Internal.Orphans where

import           Data.Aeson
import           Data.Scientific     (Scientific)
import qualified Data.Text           as T
import           Data.Time.Clock     (NominalDiffTime)
import           Network.API.Builder

instance FromJSON NominalDiffTime where
  parseJSON =
    withScientific "NominalDiffTime"
    (pure . fromIntegral . (round :: Scientific -> Integer))

instance ToJSON NominalDiffTime where
  toJSON v = toJSON (round v :: Integer)

instance Receivable Int where
  receive = useFromJSON

instance (ToQuery a, ToQuery b) => ToQuery (a, b) where
  toQuery n (a, b) = toQuery n . T.intercalate "_" $
                     ((map snd $ toQuery n a) ++ (map snd $ toQuery n b))

instance ToQuery Double where
  toQuery n v = toQuery n $ T.pack (show v)
