module D15
  ( day15A
  , day15B
  , wrap
  ) where

import           Control.Monad.State
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Foldable
import           Data.Traversable
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Array as A
import           Data.Ix
import           Data.Monoid

import           Util

day15A :: BS.ByteString -> BS.ByteString
day15A (BS.lines -> lns) =
  let rows = length lns
      cols = BS.length $ head lns
      arr = A.listArray ((0, 0), (rows-1, cols-1)) $ concatMap (map (read . pure) . BS.unpack) lns
      m = explore arr mempty (S.singleton $ Path ((0,0), 0))
   in showBS $ m M.! (rows-1, cols-1)

type M = A.Array (Int, Int) Int

newtype Path = Path ((Int, Int), Int) deriving (Show, Eq)

instance Ord Path where
  compare (Path (ca, a)) (Path (cb, b)) =
    compare a b <> compare ca cb

explore :: M -> M.Map (Int, Int) Int -> S.Set Path -> M.Map (Int, Int) Int
explore _ visited paths | S.null paths = visited
explore arr visited paths =
  let (Path p, paths') = S.deleteFindMin paths
      (newPaths, newVisited) = step arr p visited
   in explore arr newVisited $ S.fromList (Path <$> newPaths) <> paths'

step :: M ->  ((Int, Int), Int) -> M.Map (Int, Int) Int
     -> ([((Int, Int),  Int)], M.Map (Int,Int) Int)
step arr ((r, c), !risk) visited
  | (r, c) == snd (A.bounds arr) =
      ([], M.alter (maybe (Just risk) (Just . min risk)) (r, c) visited)
  | Just rk <- M.lookup (r, c) visited
  , risk >= rk = ([], visited)
  | Just final <- M.lookup (snd $ A.bounds arr) visited
  , risk >= final = ([], visited)
  | otherwise =
      let l = [ ((r,c-1), arr A.! (r,c-1)) | inRange  (A.bounds arr)  (r, c-1)]
          rs = [ ((r,c+1), arr A.! (r,c+1)) | inRange  (A.bounds arr) (r, c+1)]
          u = [ ((r-1,c), arr A.! (r-1,c)) | inRange  (A.bounds arr) (r-1, c)]
          d = [ ((r+1,c), arr A.! (r+1,c)) | inRange  (A.bounds arr) (r+1, c)]
       in (fmap (+ risk) <$> concat [l,rs,u,d], M.insert (r, c) risk visited)

day15B :: BS.ByteString -> BS.ByteString
day15B (BS.lines -> lns) =
  let rows = length lns
      cols = BS.length $ head lns
      bnd = (rows-1, cols-1)
      cells = extendMap rows cols $ range ((0,0), bnd)
                `zip` concatMap (map (read . pure) . BS.unpack) lns
      arr = A.array ((0, 0), (rows *5 -1, cols * 5 -1)) cells
      m = explore arr mempty (S.singleton $ Path ((0,0), 0))
   in showBS $ m M.! (rows * 5 -1, cols *5 -1)

extendMap :: Int -> Int -> [((Int, Int),Int)] -> [((Int, Int),Int)]
extendMap rs cs m = do
        xr <- [0..4]
        xc <- [0..4]
        ((r,c), v) <- m
        [((r + xr * rs, c + xc * cs), wrap (xr + xc + v))]

wrap :: Int -> Int
wrap x = ((x - 1) `mod` 9) + 1
