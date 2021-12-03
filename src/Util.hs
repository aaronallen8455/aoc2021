module Util
  ( readInt
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Maybe

readInt :: BS.ByteString -> Int
readInt = go . BS.readInt where
  go (Just (n, _)) = n
  go _ = error "failed to parse Int"

