module Imager3000.Concurrent.MVar
  ( concurrently
  , defaultConfig
  , poolSize
  ) where

import Control.Concurrent


data Config = Config
  { poolSize :: Int
  } deriving Show


defaultConfig :: Config
defaultConfig = Config { poolSize=5 }

-- | This function works by creating an `MVar` and putting
--   all the data as `Just datum`.
--
--   Then it spawns `poolSize` threads, which one by one consume this value.
--   If it's `Just something` they process it and iterate.
--   If it's `Nothing`, they put Nothing back in and free their lock.
concurrently :: Config -> [IO ()] -> IO ()
concurrently cfg acts = do

    -- create data pipe in
    m_source <- newEmptyMVar

    -- put all data on it, on a separate thread
    forkIO (mvarFeed acts m_source)

    -- create `poolSize` locks, in order to wait
    -- for the completition of each thread
    m_locks <- mapM (\_ -> newEmptyMVar) [1..poolSize cfg]

    -- span `poolSize` threads, all consuming from `m_source`
    mapM_ (\lock -> forkIO (mvarConsume lock m_source)) m_locks

    -- wait for threads to finish
    mapM_ takeMVar m_locks


-- | Basically, take from the second MVar until it's Nothing,
--   executing it's contents as IO, and then put () into the first one.
mvarConsume :: MVar () -> MVar (Maybe (IO ())) -> IO ()
mvarConsume m_lock m_source = do
    -- take the data
    t <- takeMVar m_source
    case t of
        -- if it has something
        Just t' -> do
            -- execute it
            t'
            -- and call ourselves
            mvarConsume m_lock m_source

        -- if it's empty
        Nothing -> do
            -- put Nothing back in
            putMVar m_source Nothing
            -- and clear the lock
            putMVar m_lock ()


mvarFeed :: [a] -> MVar (Maybe a) -> IO ()
mvarFeed [] m_source = putMVar m_source Nothing
mvarFeed (x:xs) m_source = do
    putMVar m_source (Just x)
    mvarFeed xs m_source
