module Imager3000.Concurrent.Async
  ( concurrently
  , defaultConfig
  , poolSize
  ) where

import Control.Concurrent.Async.Pool hiding (concurrently)


data Config = Config
  { poolSize :: Int
  } deriving Show


defaultConfig :: Config
defaultConfig = Config { poolSize=5 }

concurrently :: Config -> [IO ()] -> IO ()
concurrently cfg acts = do
    withTaskGroup (poolSize cfg) (
        \g -> mapTasks g acts)

    return ()
