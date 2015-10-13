module Imager3000.Fetch
  ( fetch
  ) where

import Control.Concurrent


fetch :: String -> String -> IO ()
fetch base_url url = do
    putStrLn ("* Fetching: " ++ url)
    threadDelay (1 * 1000000)
    putStrLn ("- Fetched: " ++ url)
