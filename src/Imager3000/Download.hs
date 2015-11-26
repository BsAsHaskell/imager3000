{-# LANGUAGE OverloadedStrings #-}
module Imager3000.Download
  ( download
  , getFullURI
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.HTTP.Client
import Network.URI


download :: String -> IO ByteString
download url = do
    manager <- newManager defaultManagerSettings
    request <- parseUrl url
    response <- httpLbs request manager
    return (responseBody response)


getFullURI :: String -> String -> String
getFullURI master url =
    let Just master_url = parseURIReference master
        Just url_url = parseURIReference url
    in
    case uriIsAbsolute url_url of
        True -> show url
        False -> show (relativeTo url_url master_url)
