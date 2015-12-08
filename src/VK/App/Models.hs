{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
-- | Models for views

module VK.App.Models where

import           Control.DeepSeq
import           GHC.Generics            (Generic)


import qualified VK.API                  as VK
import           VK.App.Internal.Orphans ()
import           VK.App.Widgets

data ItemsPager a = ItemsPager {
  items      :: !(VK.Items a)
  , pageSize :: !Int
  , offset   :: !Int
  }
                  deriving (Show, Generic, NFData)

instance Pagination (ItemsPager a) where
  type PageIndex (ItemsPager a) = Int
  type PageData (ItemsPager a) = [a]

  firstPage (ItemsPager items _ _) =
    if VK.count items == 0 then Nothing else Just 0
  lastPage (ItemsPager items pgsize _) =
    if VK.count items == 0 then Nothing else Just $ VK.count items - VK.count items `rem` pgsize
  currentPage (ItemsPager items _ offset) =
    if VK.count items == 0 then Nothing else Just offset
  prevPage (ItemsPager items pgsize _) idx
    | VK.count items <= 0 = Nothing
    | idx - pgsize >= 0 = Just $ idx - pgsize
    | otherwise = Nothing
  nextPage (ItemsPager items pgsize _) idx
    | VK.count items == 0 = Nothing
    | idx + pgsize >= VK.count items = Nothing
    | otherwise = Just $ idx + pgsize
  pageNum (ItemsPager _ pgsize _) idx = idx `div` pgsize + 1
  pageData (ItemsPager items _ _) =
    if VK.count items == 0
    then Nothing
    else Just $ VK.items items
