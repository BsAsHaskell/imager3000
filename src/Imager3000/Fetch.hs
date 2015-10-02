module Imager3000.Fetch
  ( fetch
  ) where


import Control.Concurrent

import Imager3000.Types

fetch :: String -> Action String
fetch base_url url = do
    putStrLn $ "Fetching: " ++ url
    threadDelay (1 * 1000000)
    putStrLn $ "Fetched: " ++ url
