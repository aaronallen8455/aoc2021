module Util
  ( readInt
  , chunksOf
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Maybe

readInt :: BS.ByteString -> Int
readInt = go . BS.readInt where
  go (Just (n, _)) = n
  go _ = error "failed to parse Int"

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ []  = []
chunksOf n (splitAt n -> (h, t)) = h : chunksOf n t

