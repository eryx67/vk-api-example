{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
-- | Data types for composable application.

module VK.App.Types where

import           React.Flux


import qualified Data.Text     as T
import           Data.Typeable (Typeable)


-- | Application name
type AppName = String
-- | Main application view
type AppView = ReactElementM ViewEventHandler
-- | Route a path to application action
type AppRouter = [T.Text] -> IO ()
-- | Construct location hash fragment for subapplication action path
type ParentRouter = Maybe ([T.Text] -> T.Text)

type ApiState = ()

-- | Datatype for application
data App props = forall state. StoreData state =>
           App {appName        :: AppName
               , appState      :: ReactStore state
                                  -- ^ application state
               , appView       :: Typeable props => state -> props -> AppView ()
                                  -- ^ application view
               , appInitAction :: StoreAction state
                                  -- ^ initialize and run application
               , appRouter     :: Maybe AppRouter
                                  -- ^ application actions router
               }
               deriving Typeable

-- | Create application view, initialize and run application
initApp :: Typeable props => App props -> IO (ReactView props)
initApp App{..} = do
  let view' = defineControllerView appName appState appView
  alterStore appState appInitAction
  return view'
