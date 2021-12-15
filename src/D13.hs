module D13
  ( day13A
  , day13B
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import qualified Data.Set as S

import Util

day13A :: BS.ByteString -> BS.ByteString
day13A (break (== "") . BS.lines -> (map parsePt -> pts, map parseFold -> _:folds)) =
  showBS . S.size . doFold (head folds) $ S.fromList pts

doFold :: Fold -> S.Set (Int, Int) -> S.Set (Int, Int)
doFold (X n) s = S.map go s where
  go (x, y)
    | x < n = (x, y)
    | otherwise = (n - (x - n), y)
doFold (Y n) s = S.map go s where
  go (x, y)
    | y < n = (x, y)
    | otherwise = (x, n - (y - n))

parsePt :: BS.ByteString -> (Int, Int)
parsePt (map readInt . BS.split ',' -> [x, y]) = (x, y)

data Fold = X Int | Y Int deriving Show

parseFold :: BS.ByteString -> Fold
parseFold (BS.split '=' . BS.drop 11 -> [axis, num]) =
  case axis of
    "y" -> Y $ readInt num
    "x" -> X $ readInt num

day13B :: BS.ByteString -> BS.ByteString
day13B (break (== "") . BS.lines -> (map parsePt -> pts, map parseFold -> _:folds)) =
  plot . flip (foldl' (flip doFold)) folds $ S.fromList pts

plot :: S.Set (Int, Int) -> BS.ByteString
plot s = BS.unlines $ fmap mconcat $ do
  x <- [0..40]
  pure $ do
    y <- [0..20]
    pure $ if S.member (x,y) s
       then "x"
       else " " :: BS.ByteString
