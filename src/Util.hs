module Util
  ( readInt
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Maybe

readInt :: BS.ByteString -> Int
readInt = fst . fromJust . BS.readInt
