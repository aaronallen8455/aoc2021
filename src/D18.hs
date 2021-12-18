module D18
  ( day18A
  , day18B
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import           Text.Parsec
import           Text.Parsec.ByteString

import           Util

data SN
  = RealN Int
  | Pair SN SN
  deriving Show

parseSN :: Parser SN
parseSN = parsePair <|> parseReal
  where
    parsePair = do
      char '['
      a <- parseSN
      char ','
      b <- parseSN
      char ']'
      pure $ Pair a b
    parseReal = do
      RealN . read . pure <$> digit

day18A :: BS.ByteString -> BS.ByteString
day18A (BS.lines -> lns) =
  let Right nums = traverse (parse parseSN "") lns
      res = L.foldl1' addNums nums
   in showBS $ mag res

mag :: SN -> Int
mag (RealN x) = x
mag (Pair a b) = (mag a * 3) + (mag b * 2)

addNums :: SN -> SN -> SN
addNums a b = reduce $ Pair a b

reduce :: SN -> SN
reduce sn =
  case explode sn of
    (Nil, _) ->
      case split sn of
        (SDone, sn') -> reduce sn'
        (SLook, _) -> sn
    (_, sn') -> reduce sn'

data ExplR
  = L Int
  | R Int
  | LR Int Int
  | Done
  | Nil

explode :: SN -> (ExplR, SN)
explode = go 0 where
  go 4 (Pair (RealN a) (RealN b)) = (LR a b, RealN 0)
  go 4 p = (Nil, p)
  go _ p@(RealN _) = (Nil, p)
  go n (Pair a b) =
    case go (n + 1) a of
      (L x, a') -> (L x, Pair a' b)
      (R x, a') -> (Done, Pair a' (addLeftMost x b))
      (LR x y, a') -> (L x, Pair a' (addLeftMost y b))
      (Done, a') -> (Done, Pair a' b)
      (Nil, _) ->
        case go (n + 1) b of
          (L x, b') -> (Done, Pair (addRightMost x a) b')
          (R x, b') -> (R x, Pair a b')
          (LR x y, b') -> (R y, Pair (addRightMost x a) b')
          (Done, b') -> (Done, Pair a b')
          (Nil, _) -> (Nil, Pair a b)

  addLeftMost x (Pair a b) = Pair (addLeftMost x a) b
  addLeftMost x (RealN y) = RealN $ x + y

  addRightMost x (Pair a b) = Pair a (addRightMost x b)
  addRightMost x (RealN y) = RealN $ x + y

data SplitRes
  = SDone
  | SLook

split :: SN -> (SplitRes, SN)
split (RealN x)
  | x >= 10 = (SDone, let d = div x 2 in Pair (RealN d) (RealN (x - d)))
  | otherwise = (SLook, RealN x)
split (Pair a b) =
  case split a of
    (SDone, a') -> (SDone, Pair a' b)
    (SLook, _) ->
      case split b of
        (SDone, b') -> (SDone, Pair a b')
        (SLook, _) -> (SLook, Pair a b)

day18B :: BS.ByteString -> BS.ByteString
day18B (BS.lines -> lns) =
  let Right nums = traverse (parse parseSN "") lns
      res = maximum $ do
        i <- [0..length nums - 1]
        let (pre, a: rest) = splitAt i nums
        b <- pre ++ rest
        pure $ mag $ addNums a b

   in showBS res
