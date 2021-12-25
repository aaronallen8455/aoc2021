module D25
  ( day25A
  , day25B
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import           Data.Monoid

import           Util

day25A :: BS.ByteString -> BS.ByteString
day25A (BS.lines -> lns) =
  let rows = length lns
      cols = BS.length $ head lns
      init = M.fromList $ do
        (ri, row) <- zip [0..rows-1] lns
        (ci, col) <- zip [0..cols-1] (BS.unpack row)
        pure ((ri, ci), toCell col)
      res = length
          . takeWhile ((/= 0) . fst)
          $ iterate (step . snd) (1, init)
   in showBS res

type Grid = M.Map (Int,Int) Cell

step :: Grid -> (Sum Int, Grid)
step m =
  let (n, m') = move East m
      (n', res) = move South m'
   in (n + n', res)

move :: Cell -> Grid -> (Sum Int, Grid)
move dir m =
  let ((upds, cnt), m') = M.traverseWithKey go m
      go (row, col) c | c /= dir = (([], 0) ,c)
      go (row, col) c =
        let nextCoord =
              case c of
                East ->
                  case M.lookup (row, col+1) m of
                    Nothing ->
                      case M.lookup (row, 0) m of
                        Just Empty -> Just (row, 0)
                        _ -> Nothing
                    Just Empty -> Just (row, col+1)
                    _ -> Nothing
                South ->
                  case M.lookup (row+1, col) m of
                    Nothing ->
                      case M.lookup (0, col) m of
                        Just Empty -> Just (0,col)
                        _ -> Nothing
                    Just Empty -> Just (row+1, col)
                    _ -> Nothing
         in case nextCoord of
              Nothing -> (([], 0), c)
              Just coord -> (([(coord, c)], 1), Empty)
   in (cnt, M.union (M.fromList upds) m')


toCell :: Char -> Cell
toCell = \case
  'v' -> South
  '>' -> East
  '.' -> Empty

data Cell
  = East
  | South
  | Empty
  deriving (Show, Eq)

day25B :: BS.ByteString -> BS.ByteString
day25B = undefined
