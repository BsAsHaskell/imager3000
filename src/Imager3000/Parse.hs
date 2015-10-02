{-# LANGUAGE OverloadedStrings #-}
module Imager3000.Parse
  ( getImages
  ) where

import Text.Taggy.DOM
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, unpack)
import Data.Text.Lazy.Encoding
import Data.Text.Encoding.Error
import qualified Data.HashMap.Strict as M


getImages :: ByteString -> [String]
getImages contents =
    let txt = decodeUtf8With ignore contents
        dom = parseDOM True txt
    in
    map unpack (findImages dom)


findImages :: [Node] -> [Text]
findImages vals = concatMap extract vals
  where
    extract (NodeContent _) = []
    extract (NodeElement el@(Element _ _ childs)) = getImg el ++ findImages childs
    getImg (Element "img" attrs _) = case M.lookup "src" attrs of
                                         Just x -> [x]
                                         Nothing -> []
    getImg (Element _ _ _) = []
