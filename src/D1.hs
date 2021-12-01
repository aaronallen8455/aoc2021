module D1
  ( day1A
  , day1B
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import           Data.Maybe

import           Util

day1A :: BS.ByteString -> BS.ByteString
day1A bs =
  let depths = readInt <$> BS.lines bs
   in BS.pack . show $ solve depths

day1B :: BS.ByteString -> BS.ByteString
day1B bs =
  let depths = readInt <$> BS.lines bs
      grouped =
        zipWith3 (((+) .) . (+))
          depths
          (tail depths)
          (drop 2 depths)
   in BS.pack . show $ solve grouped

solve :: [Int] -> Int
solve depths =
  let go acc (x, y)
        | y > x = succ acc
        | otherwise = acc
   in List.foldl' go 0 (zip depths $ tail depths)
