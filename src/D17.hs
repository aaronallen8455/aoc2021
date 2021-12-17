module D17
  ( day17A
  , day17B
  ) where

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.Ix

import           Util

day17A :: BS.ByteString -> BS.ByteString
day17A (BS.break (not . isNumber) . BS.tail . snd . BS.break (== '=') -> (x1, rest)) =
  let (_x2, rest') = BS.span isNumber $ BS.drop 2 rest
      (_, (y1, _)) = BS.break (== '.') . BS.tail <$> BS.break (== '=') rest'
   in showBS $ peak (abs $ readInt y1 + 1)

peak :: Int -> Int
peak y = maximum $ scanl1 (+) [y,y-1..0]

day17B :: BS.ByteString -> BS.ByteString
day17B (BS.break (not . isNumber) . BS.tail . snd . BS.break (== '=') -> (x1, rest)) =
  let (x2, rest') = BS.span isNumber $ BS.drop 2 rest
      (_, (y1, rest'')) = BS.break (== '.') . BS.tail <$> BS.break (== '=') rest'
      y2 = BS.init $  BS.dropWhile (== '.') rest''
      xB = (readInt x1, readInt x2)
      bnds = (xB, (readInt y1, readInt y2))
      xP = xPossible xB
      yP = yPossible bnds xP
   in showBS $ length yP

xPossible :: (Int, Int) -> [Int]
xPossible (s, e) = filter (hits 0) pos
  where
    pos = [e,e-1 .. 0]
    hits !x !v
      | x >= s, x <= e = True
      | x > e || v == 0 = False
      | otherwise = hits (x + v) (max 0 $ pred v)

yPossible :: ((Int, Int), (Int, Int)) -> [Int] -> [()]
yPossible bnds@(xBound, (s, e)) xPos = do
  xp <- xPos :: [Int]
  let i = abs $ s+1
  poss <- [i, i -1 .. s] :: [Int]
  guard $ test (0, 0) xp poss
  pure ()
  where
    test :: (Int, Int) -> Int -> Int -> Bool
    test (x, y) xv yv
      | inRange xBound x, inRange (s, e) y = True
      | y < s = False
      | x > snd xBound = False
      | otherwise = test (x + xv, y + yv) (max 0 $ xv - 1) (yv - 1)

