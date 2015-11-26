module Imager3000.Concurrent.MVar
  ( concurrently
  , defaultConfig
  , poolSize
  ) where

import Control.Concurrent
import Control.Monad


data Config = Config
  { poolSize :: Int
  } deriving Show


defaultConfig :: Config
defaultConfig = Config { poolSize=5 }

-- | Esta función toma una lista de `actions` IO y las corre
--   concurrentemente usando `poolSize` threads.
concurrently :: Config -> [IO ()] -> IO ()
concurrently cfg actions = do

    -- Primero creamos una MVar vacia.
    m_source <- newEmptyMVar

    -- Después en una thread, metemos cada acción
    -- en la MVar secuencialmente.
    forkIO (do
        forM_ actions (\act -> do
            putMVar m_source (Just act)
            )
        -- y al final ponemos Nothing para desencadenar
        -- el fin de cada thread del pool
        putMVar m_source Nothing
        )

    -- Por otro lado, creamos `poolSize` MVars, funcionando como locks,
    -- para luego esperar a que cada thread termine.
    m_locks <- forM [1..poolSize cfg] (\_ -> do
        newEmptyMVar
        )

    -- Finalmente levantamos una thread por lock
    -- que consuma la MVar
    forM_ m_locks (\lock -> do
        forkIO (mvarConsume lock m_source)
        )

    -- y esperamos que todos los locks se liberen
    forM_ m_locks (\l -> do
        takeMVar l
        )


-- | Recursivamente tomar de `m_source` un `Maybe (IO ())`:
--      si tiene `Just algo`, ejecutar `algo`
--      si tiene `Nothing`, liberar el lock `m_lock` poniendo `()`.
mvarConsume :: MVar () -> MVar (Maybe (IO ())) -> IO ()
mvarConsume m_lock m_source = do
    t <- takeMVar m_source
    case t of
        Just t' -> do
            t'
            mvarConsume m_lock m_source

        Nothing -> do
            -- si no ponemos `Nothing` de nuevo, la próxima
            -- thread del pool que quiera usar la MVar
            -- lockearia indefinidamente
            putMVar m_source Nothing

            -- finalmente releaseamos el lock
            putMVar m_lock ()
