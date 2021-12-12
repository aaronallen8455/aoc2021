module D12
  ( day12A
  , day12B
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import           Data.Char
import qualified Data.Set as S
import           Data.Tuple
import qualified Data.List as L

import           Util

day12A :: BS.ByteString -> BS.ByteString
day12A (BS.lines -> lns) =
  let m = toMap $ parseLine <$> lns
   in showBS $ travel m [(Small "start", S.singleton "start")]

type CM = M.Map Cave (S.Set Cave)

toMap :: [(Cave, Cave)] -> CM
toMap edges = M.fromListWith (<>) $ do
  e <- edges
  [fmap S.singleton e, S.singleton <$> swap e]

data Cave
  = Big BS.ByteString
  | Small BS.ByteString
  deriving (Eq, Ord, Show)

parseLine :: BS.ByteString -> (Cave, Cave)
parseLine (BS.break (== '-') -> (from, BS.tail -> to)) =
  (toCave from, toCave to)

travel :: CM -> [(Cave, S.Set BS.ByteString)] -> Int
travel _ [] = 0
travel m branches =
  let (finished, unfinished) = L.partition ((== Small "end") . fst) branches
      newBranches = concatMap (branch m) unfinished
   in length finished + travel m newBranches

branch :: CM -> (Cave, S.Set BS.ByteString) -> [(Cave, S.Set BS.ByteString)]
branch m (c, visited)
  | Just edges <- M.lookup c m
  = do
    e <- S.toList edges
    newVisited <-
      case e of
        Small bs | S.member bs visited -> []
                 | otherwise -> [S.insert bs visited]
        _ -> [visited]
    [(e, newVisited)]

toCave :: BS.ByteString -> Cave
toCave bs
  | all isLower $ BS.unpack bs = Small bs
  | otherwise = Big bs

day12B :: BS.ByteString -> BS.ByteString
day12B (BS.lines -> lns) =
  let m = toMap $ parseLine <$> lns
   in showBS $ travel' m [(Small "start", S.singleton "start", False)]

_1 :: (a,b,c) -> a
_1 (a,_,_) = a

travel' :: CM -> [(Cave, S.Set BS.ByteString, Bool)] -> Int
travel' _ [] = 0
travel' m branches =
  let (finished, unfinished) = L.partition ((== Small "end") . _1) branches
      newBranches = concatMap (branch' m) unfinished
   in length finished + travel' m newBranches

branch' :: CM -> (Cave, S.Set BS.ByteString, Bool) -> [(Cave, S.Set BS.ByteString, Bool)]
branch' m (c, visited, doubled)
  | Just edges <- M.lookup c m
  = do
    e <- S.toList edges
    case e of
      Small bs ->
        if S.member bs visited
           then [(e, visited, True) | not doubled, bs /= "start" ]
           else [(e, S.insert bs visited, doubled)]
      Big _ ->
        [(e, visited, doubled)]
