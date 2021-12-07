module D6
  ( day6A
  , day6B
  ) where

import qualified Data.Array as A
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import qualified Data.List as L
import Util
import qualified Data.IntMap as IM

day6A :: BS.ByteString -> BS.ByteString
day6A (A.accumArray (+) 0 (0, 8) . map ((,1) .readInt) . BS.split ',' -> input)
  = BS.pack . show . sum . (!! 80)
  $ iterate step input

day6B :: BS.ByteString -> BS.ByteString
day6B (A.accumArray (+) 0 (0, 8) . map ((,1) .readInt) . BS.split ',' -> input)
  = BS.pack . show . sum . (!! 3000)
  $ iterate step input

step :: A.Array Int Int -> A.Array Int Int
step m = A.accumArray (+) 0 (0, 8) $ do
  (i, x) <- A.assocs m
  if i == 0
     then [(6, x), (8, x)]
     else [(i - 1, x)]
