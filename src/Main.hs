{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment

import Imager3000.Parse
import Imager3000.Download
import Imager3000.Concurrent.Async
import Imager3000.Fetch


main :: IO ()
main = do
    [url] <- getArgs
    run url

run :: String -> IO ()
run base_url = do
    contents <- download base_url

    let imgs = getImages contents

    print imgs

    let action = \url -> fetch base_url url

    concurrently defaultConfig imgs action
