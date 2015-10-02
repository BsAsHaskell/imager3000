module Imager3000.Concurrent.Async
  ( concurrently
  , defaultConfig
  , poolSize
  ) where

import Control.Concurrent
import Control.Concurrent.Async.Pool hiding (concurrently)

import Imager3000.Types


data Config = Config
  { poolSize :: Int
  } deriving Show


defaultConfig :: Config
defaultConfig = Config { poolSize=5 }

concurrently :: Show a => Config -> [a] -> Action a -> IO ()
concurrently cfg datas act = do
    let actions = map act datas

    withTaskGroup (poolSize cfg) (
        \g -> mapTasks g actions)

    return ()
