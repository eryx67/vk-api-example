{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
-- | Common widgets for views

module VK.App.Widgets.Pagination where

import           Control.Monad (join)
import           Data.Foldable (forM_)
import           Data.List     (unfoldr)
import           Data.Maybe    (fromMaybe)
import qualified Data.Text     as T

import           React.Flux

class Pagination pg where
  type PageIndex pg :: *
  type PageData pg :: *
  firstPage :: pg -> Maybe (PageIndex pg)
  lastPage :: pg -> Maybe (PageIndex pg)
  currentPage :: pg -> Maybe (PageIndex pg)
  prevPage :: pg -> PageIndex pg -> Maybe (PageIndex pg)
  nextPage :: pg -> PageIndex pg -> Maybe (PageIndex pg)
  pageNum :: pg -> PageIndex pg -> Int
  pageData :: pg -> Maybe (PageData pg)

pagerView :: (Pagination pg, Eq (PageIndex pg), Show (PageIndex pg)) =>
             pg -> Int
             -> (PageData pg -> ReactElementM ViewEventHandler ())
             -> (PageIndex pg -> T.Text)
             -> ReactElementM ViewEventHandler ()
pagerView pg navLen dataView pageAction = do
  let curPage = currentPage pg
      mps = pages <$> curPage
      (prevHide, nextHide) =
        fromMaybe (True, True) $ do
          ps <- mps
          if null ps
            then
            return (True, True)
            else
            return (firstPage pg == Just (head ps),
                    lastPage pg == Just (last ps))
      pageIdxs = fromMaybe [] mps
      prevPageAction = maybe "" pageAction (join $ prevPage pg <$> curPage)
      nextPageAction = maybe "" pageAction (join $ nextPage pg <$> curPage)
  div_ $ do
    maybe mempty dataView $ pageData pg
    nav_ $
      ul_ ["className" $= "pagination"] $ do
        li_ ["className" $= "disabled" | prevHide] $
          if prevHide
          then
            span_ ["aria-hidden" $= "true"] $ elemText "<<"
          else
            a_ ["href" $= prevPageAction
               , "aria-label" $= "Previous"] $
            span_ [] $ elemText "<<"
        forM_ pageIdxs (\pidx ->
                          li_ ["className" $= "active" | Just pidx == curPage] $
                          a_ ["href" $= pageAction pidx] $
                          elemShow $ pageNum pg pidx)

        li_ ["className" $= "disabled" | nextHide] $
          if nextHide
          then
            span_ ["aria-hidden" $= "true"] $ elemText ">>"
          else
            a_ ["href" $= nextPageAction
               , "aria-label" $= "Next"] $
            span_ [] $ elemText ">>"
  where
    pages p =
      let pps = reverse . take (navLen `div` 2  - 1) $ prevPages p navLen
          nps = nextPages p (navLen - length pps - 1)
      in
        pps ++ p:nps

    findPages nxt p len =
      unfoldr (\(l, cp) ->
                 if null l
                 then Nothing
                 else (\np -> (np , (tail l, np))) <$> nxt cp)
      ([1..len], p)
    prevPages p = findPages (prevPage pg) p
    nextPages = findPages (nextPage pg)
