{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeSynonymInstances     #-}
-- |

module Main where

#ifdef __GHCJS__

import           React.Flux

import qualified VK.App                  as VK

main :: IO ()
main = do
  av <- VK.initApp VK.app
  case VK.app of
    VK.App{VK.appRouter = Just ar} -> VK.initRouter ar
    _ -> return ()
  reactRender "vk-app" av Nothing

#else
main :: IO ()
main = return ()
#endif
