module D11
  ( day11A
  , day11B
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Foldable
import qualified Data.Map.Strict as M
import           Data.Ix

import           Util

day11A :: BS.ByteString -> BS.ByteString
day11A (BS.lines -> lns) =
  let m = M.fromList
        $ zip (range ((0,0), (9,9)))
              (concatMap (map (Just . read . pure) . BS.unpack) lns)
   in showBS . sum . take 100 $ step m

type M = M.Map (Int,Int) (Maybe Int)

step :: M -> [Int]
step m =
  let m' = fmap succ <$> m :: M
      changes m = do
        ((r, c), lvl) <- M.toList m
        if lvl > Just 9
           then (((r, c), const Nothing) :) $ do
             r' <- [r-1,r,r+1]
             c' <- [c-1,c,c+1]
             [((r',c'), fmap (+1))]
           else []

      applyChanges m =
        let cs = changes m
         in foldl' (\m (cord, change) -> M.adjust change cord m) m cs

      fixed m = all (<= Just 9) m

      finalM = until fixed applyChanges m' :: M
      numFlashes = length . filter (== Nothing) $ M.elems finalM
      m'' = fmap (maybe (Just 0) Just) finalM
   in numFlashes : step m''

day11B :: BS.ByteString -> BS.ByteString
day11B (BS.lines -> lns) =
  let m = M.fromList
        $ zip (range ((0,0), (9,9)))
              (concatMap (map (Just . read . pure) . BS.unpack) lns)
   in showBS . succ . length . takeWhile (/= 100) $ step m
