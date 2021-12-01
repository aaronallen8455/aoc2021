module Util
  ( readInt
  ) where

import qualified Data.ByteString.Char8 as BS

readInt :: BS.ByteString -> Maybe Int
readInt = fmap fst . BS.readInt
