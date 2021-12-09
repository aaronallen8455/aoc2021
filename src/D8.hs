module D8
  ( day8A
  , day8B
  ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Tuple
import           Data.Maybe

import           Debug.Trace

day8A :: BS.ByteString -> BS.ByteString
day8A (map (BS.words . BS.tail . snd . BS.break (== '|')) . BS.lines -> inp) =
  BS.pack . show . sum $ map (length . filter uniqueDigit) inp

uniqueDigit :: BS.ByteString -> Bool
uniqueDigit bs =
  case BS.length bs of
    2 -> True
    4 -> True
    3 -> True
    7 -> True
    _ -> False

day8B :: BS.ByteString -> BS.ByteString
day8B (map ((\(x,y) -> (BS.words x, BS.words $ BS.tail y)) . BS.break (== '|')) . BS.lines -> inp)
  = BS.pack . show . sum
  $ map (\(i, o) -> read . fromJust $ solves (findConnections i) o)
        inp

solves :: [M.Map Char Char] -> [BS.ByteString] -> Maybe String
solves ms bs = asum $ do
  m <- ms
  pure . fmap concat $ traverse (solve m) bs

solve :: M.Map Char Char -> BS.ByteString -> Maybe String
solve m o =
  fmap show . matchDigit . S.fromList . mapMaybe (`M.lookup` m) $ BS.unpack o

matchDigit :: S.Set Char -> Maybe Int
matchDigit s =
  case S.toList s of
    "abcefg" -> Just 0
    "cf" -> Just 1
    "acdeg" -> Just 2
    "acdfg" -> Just 3
    "bcdf" -> Just 4
    "abdfg" -> Just 5
    "abdefg" -> Just 6
    "acf" -> Just 7
    "abcdefg" -> Just 8
    "abcdfg" -> Just 9
    _ -> Nothing

findConnections :: [BS.ByteString] -> [M.Map Char Char]
findConnections input
    = map revMap
    . collectMappings
    $ foldl' go mempty input
  where
    revMap = M.fromList . map swap .  M.toList

    go m (BS.unpack -> bs)
      = M.unionWith S.intersection m . M.fromList $ do
      c <- bs
      pos <- case length bs of
        2 -> ["cf"]
        4 -> ["bcdf"]
        3 -> ["acf"]
        _ -> ["abcdefg"]
      pure (c, S.fromList pos)

collectMappings :: M.Map Char (S.Set Char) -> [M.Map Char Char]
collectMappings = foldM possibleMappings mempty . M.toList

possibleMappings :: M.Map Char Char -> (Char, S.Set Char) -> [M.Map Char Char]
possibleMappings acc (c, pos) = do
  p <- S.toList pos
  guard . not $ M.member p acc
  pure $ M.insert p c acc
