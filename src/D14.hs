module D14
  ( day14A
  , day14B
  ) where

import           Data.Bifunctor
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.Map.Strict as M

import           Util

day14A :: BS.ByteString -> BS.ByteString
day14A (break (== "") . BS.lines -> ([temp], _:rs)) =
  let rules = M.fromList $ parseRule <$> rs
      startPairs = M.fromListWith (+) . map (,1) $ BS.zip temp (BS.drop 1 temp)
   in showBS . solve . L.sort
      . unMap (BS.head temp) (BS.last temp) . (!! 10)
      $ iterate (applyRules rules) startPairs

solve :: [Int] -> Int
solve xs = last xs - head xs

day14B :: BS.ByteString -> BS.ByteString
day14B (break (== "") . BS.lines -> ([temp], _:rs)) =
  let rules = M.fromList $ parseRule <$> rs
      startPairs = M.fromListWith (+) . map (,1) $ BS.zip temp (BS.drop 1 temp)
   in showBS . solve . L.sort
      . unMap (BS.head temp) (BS.last temp) . (!! 40)
      $ iterate (applyRules rules) startPairs

parseRule :: BS.ByteString -> ((Char, Char), Char)
parseRule (bimap BS.unpack (BS.unpack . BS.drop 4) . BS.splitAt 2 -> ([a,b], [c]))
  = ((a,b), c)

applyRules :: M.Map (Char, Char) Char -> M.Map (Char, Char) Int -> M.Map (Char, Char) Int
applyRules rules m = M.fromListWith (+) $ do
  (k@(a,b), v) <- M.toList m
  let r = rules M.! k
  [((a,r), v), ((r,b), v)]

unMap :: Char -> Char -> M.Map (Char, Char) Int -> [Int]
unMap f l m = M.elems . fmap (`div` 2) . M.adjust succ l . M.adjust succ f . M.fromListWith (+) $ do
  ((a, b), v) <- M.toList m
  [(a,v), (b,v)]
