module D9
  ( day9A
  , day9B
  ) where

import           Control.Monad.State
import qualified Data.Array as A
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.List as L

import           Util

type Map = A.Array (Int, Int) Int

day9A :: BS.ByteString -> BS.ByteString
day9A (BS.lines -> lns) =
  let numRows = length lns
      numCols = BS.length $ head lns
      arr = A.listArray ((0, 0), (numRows-1, numCols -1))
          $ concatMap (map (read . pure) . BS.unpack) lns :: A.Array (Int, Int) Int
      res = sum . catMaybes $ do
        r <- [0..numRows - 1]
        c <- [0..numCols - 1]
        pure $ checkAdj arr (r, c)
   in BS.pack $ show res

checkAdj :: Map -> (Int, Int) -> Maybe Int
checkAdj arr (row, col) =
  let above
        | col > 0 = arr A.! (row, col - 1)
        | otherwise = maxBound
      below
        | col < maxCol = arr A.! (row, col + 1)
        | otherwise = maxBound
      left
        | row > 0 = arr A.! (row - 1, col)
        | otherwise = maxBound
      right
        | row < maxRow = arr A.! (row + 1, col)
        | otherwise = maxBound
      center = arr A.! (row, col)
   in if and [center < above, center < below, center < left, center < right]
         then Just $ center + 1
         else Nothing
  where
    (_, (maxRow, maxCol)) = A.bounds arr

day9B :: BS.ByteString -> BS.ByteString
day9B (BS.lines -> lns) =
  let numRows = length lns
      numCols = BS.length $ head lns
      arr = A.listArray ((0, 0), (numRows-1, numCols -1))
          $ concatMap (map (read . pure) . BS.unpack) lns :: A.Array (Int, Int) Int
      lowPoints = do
        r <- [0..numRows - 1]
        c <- [0..numCols - 1]
        [(r, c) | isJust $ checkAdj arr (r, c)]
      basins = reverse . L.sort $ do
        (r, c) <- lowPoints
        let (size, _) = findBasinSize arr (r, c) mempty
        pure size
      res = product $ take 3 basins
   in BS.pack $ show res

findBasinSize :: Map -> (Int, Int) -> S.Set (Int, Int) -> (Int, S.Set (Int, Int))
findBasinSize m (r, c) visited
  | r < 0 || r > maxRow || c < 0 || c > maxCol = (0, visited)
  | S.member (r, c) visited = (0, visited)
  | m A.! (r, c) == 9 = (0, S.insert (r, c) visited)
  | otherwise =
    let newVisited = S.insert (r, c) visited
     in (`runState` newVisited) . fmap ((+1) . sum) . traverse state $
         [ findBasinSize m (r+1, c)
         , findBasinSize m (r, c-1)
         , findBasinSize m (r, c+1)
         , findBasinSize m (r-1, c)
         ]
  where
    (_, (maxRow, maxCol)) = A.bounds m
