module D6
  ( day6A
  , day6B
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import qualified Data.List as L
import Util
import qualified Data.IntMap as IM

day6A :: BS.ByteString -> BS.ByteString
day6A (IM.fromListWith (+) . map ((,1) .readInt) . BS.split ',' -> input)
  = BS.pack . show . sum . (!! 80)
  $ iterate step input

day6B :: BS.ByteString -> BS.ByteString
day6B (IM.fromListWith (+) . map ((,1) .readInt) . BS.split ',' -> input)
  = BS.pack . show . sum . (!! 256)
  $ iterate step input

step :: IM.IntMap Int -> IM.IntMap Int
step m = IM.fromListWith (+) $ do
  (i, x) <- IM.toList m
  if i == 0
     then [(6, x), (8, x)]
     else [(i - 1, x)]
