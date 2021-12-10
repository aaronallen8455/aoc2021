module D10
  ( day10A
  , day10B
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Foldable
import           Data.Maybe
import qualified Data.List as L

import           Util

day10A :: BS.ByteString -> BS.ByteString
day10A (BS.lines -> lns) =
  showBS . sum . map toPoints $ mapMaybe (checkLine . BS.unpack) lns

checkLine :: String -> Maybe Char
checkLine = go [] where
  go _ [] = Nothing
  go [] (c : rest) | isOpen c = go [c] rest
  go [] _ = Nothing
  go (s:stack) (c : rest)
    | isOpen c = go (c:s:stack) rest
    | toCompl s == c = go stack rest
    | otherwise = Just c

toPoints :: Char -> Int
toPoints = \case
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137

toCompl :: Char -> Char
toCompl = \case
  '(' -> ')'
  '[' -> ']'
  '{' -> '}'
  '<' -> '>'

isOpen :: Char -> Bool
isOpen = (`elem` ("([{<" :: String))

day10B :: BS.ByteString -> BS.ByteString
day10B (BS.lines -> lns)
  = showBS . middleIx . L.sort . map score
  $ mapMaybe (findCompletion . BS.unpack) lns

middleIx :: [a] -> a
middleIx xs = xs !! (length xs `div` 2)

score :: String -> Int
score = foldl' go 0 where
  go s c = s * 5 + toAdd
    where
      toAdd = case c of
                ')' -> 1
                ']' -> 2
                '}' -> 3
                '>' -> 4

findCompletion :: String -> Maybe String
findCompletion = go [] where
  go stack [] = Just $ map toCompl stack
  go [] (c : rest) | isOpen c = go [c] rest
  go [] _ = Nothing
  go (s:stack) (c : rest)
    | isOpen c = go (c:s:stack) rest
    | toCompl s == c = go stack rest
    | otherwise = Nothing
