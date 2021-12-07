module D7
  ( day7A
  , day7B
  ) where

import qualified Data.ByteString.Char8 as BS

import           Util

day7A :: BS.ByteString -> BS.ByteString
day7A (map readInt . BS.split ',' -> crabs) =
  let amts = sum . map abs <$> iterate step crabs
   in BS.pack . show $ inflection amts

step :: [Int] -> [Int]
step = map pred

inflection :: [Int] -> Int
inflection amts = go $ amts `zip` tail amts where
  go ((a, b) : rest)
    | a >= b = go rest
    | otherwise = a

day7B :: BS.ByteString -> BS.ByteString
day7B (map readInt . BS.split ',' -> crabs) =
  let amts = sum <$> step2 crabs
   in BS.pack . show $ inflection amts

step2 :: [Int] -> [[Int]]
step2 crabs =
  let costs = map (consecutiveSum . abs) crabs
      newCrabs = map pred crabs
   in costs : step2 newCrabs

consecutiveSum :: Int -> Int
consecutiveSum n = (n ^ 2 + n) `div` 2
