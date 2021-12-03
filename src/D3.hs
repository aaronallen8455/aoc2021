module D3
  ( day3A
  , day3B
  , fromBin
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List

import           Util

day3A :: BS.ByteString -> BS.ByteString
day3A bs =
  let gamma = map calcGamma . BS.transpose $ BS.lines bs :: String
      epsilon = map (\case {'0' -> '1'; '1' -> '0' }) gamma
   in BS.pack . show $ fromBin gamma * fromBin epsilon

fromBin :: String -> Int
fromBin = List.foldl' go 0 where
  go acc '0' = acc * 2
  go acc '1' = acc * 2 + 1

calcGamma :: BS.ByteString -> Char
calcGamma bs =
  let num1s = length $ BS.elemIndices '1' bs
      num0s = BS.length bs - num1s
   in if num1s > num0s
         then '1'
         else '0'

day3B :: BS.ByteString -> BS.ByteString
day3B bs =
  let lns = BS.lines bs
      oxy = solve (>=) 0 lns
      co2 = solve (<) 0 lns
   in BS.pack . show $ fromBin (BS.unpack oxy) * fromBin (BS.unpack co2)

solve :: (Int -> Int -> Bool) -> Int -> [BS.ByteString] -> BS.ByteString
solve _ _ [x] = x
solve comp i bs = solve comp (i + 1) (filter f bs) where
  num1s = length . filter (== '1') $ map (`BS.index` i) bs
  num0s = length bs - num1s
  mostCommon = if num1s `comp` num0s then '1' else '0'
  f b = BS.index b i == mostCommon
