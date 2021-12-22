module D22
  ( day22A
  , day22B
  ) where

import           Data.Bifunctor
import qualified Data.ByteString.Char8 as BS
import           Data.Ix
import           Data.Foldable

import           Util

day22A :: BS.ByteString -> BS.ByteString
day22A (BS.lines -> lns) =
  let steps = reverse $ parseLn <$> lns
   in showBS $ numOn (-50, 50) (-50, 50) (-50, 50) steps

data Step = Step Bool (Int, Int) (Int, Int) (Int, Int)
  deriving Show

overlap :: (Int, Int) -> (Int, Int) -> Bool
overlap a b = or $ do
  (r1, r2) <- [(a,b), (b,a)]
  pure $ inRange r1 (fst r2) || inRange r1 (snd r2)

numOn :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [Step] -> Int
numOn _ _ _ [] = 0
numOn xr yr zr ss@(Step op sx sy sz : step)
  | not overlaps = numOn xr yr zr step
  | otherwise = sum $ do
      (x, y, z) <- breakUp
      pure $ if stepContains x y z
         then if op then rangeSize x * rangeSize y * rangeSize z
                    else 0
         else numOn x y z step
  where
    stepContains x y z =
      and [ inRange sx (fst x)
          , inRange sx (snd x)
          , inRange sy (fst y)
          , inRange sy (snd y)
          , inRange sz (fst z)
          , inRange sz (snd z)
          ]
    overlaps = overlap xr sx && overlap yr sy && overlap zr sz

    breakUp = do
      x <- breakRange xr sx
      y <- breakRange yr sy
      z <- breakRange zr sz
      pure (x, y, z)

    breakRange a b = filter ((/= 0) . rangeSize) $ breakRange' a b
    breakRange' searchRange stepRange
      | stepSInSearch && stepEInSearch =
          [(fst searchRange, fst stepRange - 1), stepRange, (snd stepRange + 1, snd searchRange)]
      | searchSInStep && searchEInStep = [searchRange]
      | stepSInSearch && searchEInStep =
          [(fst searchRange, fst stepRange - 1), (fst stepRange, snd searchRange)]
      | otherwise = -- searchSInStep && stepEInSearch =
          [(fst searchRange, snd stepRange), (snd stepRange + 1, snd searchRange)]
      where
        stepSInSearch = inRange searchRange (fst stepRange)
        stepEInSearch = inRange searchRange (snd stepRange)
        searchSInStep = inRange stepRange (fst searchRange)
        searchEInStep = inRange stepRange (snd searchRange)

parseLn :: BS.ByteString -> Step
parseLn (BS.words ->
  [op, map
         ( bimap readInt readInt
         . fmap (BS.drop 2)
         . BS.break (== '.') . BS.tail . snd . BS.break (== '=')
         ) . BS.split ','
     -> [xr, yr, zr]]) =
  Step (op == "on") xr yr zr


day22B :: BS.ByteString -> BS.ByteString
day22B (BS.lines -> lns) =
  let steps = reverse $ parseLn <$> lns
      (xr, yr, zr) = findRanges steps
   in showBS $ numOn xr yr zr steps

findRanges :: [Step] -> ((Int,Int), (Int,Int), (Int,Int))
findRanges = foldl' go ((maxBound, minBound), (maxBound, minBound), (maxBound, minBound))
  where
    go ((xs,xe), (ys,ye), (zs,ze)) (Step _ (sxs,sxe) (sys, sye) (szs, sze))
      = ((min xs sxs, max xe sxe), (min ys sys, max ye sye), (min zs szs, max ze sze))
