module D21
  ( day21A
  , day21B
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M

import           Util

day21A :: BS.ByteString -> BS.ByteString
day21A (map (map BS.tail . BS.split ':') . BS.lines -> [[_, readInt -> p1],[_, readInt -> p2]]) =
  let win ((_,s1), (_,s2), _) = s1 >= 1000 || s2 >= 1000
      ((_, score1), (_, score2), numRolls) =
        until win doRoll ((p1, 0), (p2, 0), 0)
   in showBS $ if score1 < score2
         then numRolls * score1
         else numRolls * score2

doRoll :: ((Int, Int), (Int, Int), Int) -> ((Int, Int), (Int, Int), Int)
doRoll ((!pos1, !score1), (!pos2, !score2), numRolls) =
  let dice = mod numRolls 100 + 1
      newPos1 = step dice score1 pos1
      dice' = mod (numRolls + 3) 100 + 1
      newPos2 = step dice' score2 pos2
      newScore1 = score1 + newPos1
   in if newScore1 >= 1000
         then ((newPos1, score1 + newPos1), (pos2, score2), numRolls + 3)
         else ((newPos1, score1 + newPos1),
               (newPos2, score2 + newPos2), numRolls + 6)

mod100 x = mod (x - 1) 100 + 1
mod10 x = mod (x - 1) 10 + 1

step :: Int -> Int -> Int -> Int
step curDice curScore curPos =
  let r1 = curDice
      r2 = mod100 $ curDice + 1
      r3 = mod100 $ curDice + 2
   in mod10 $ curPos + r1 + r2 + r3

day21B :: BS.ByteString -> BS.ByteString
day21B (map (map BS.tail . BS.split ':') . BS.lines -> [[_, readInt -> p1],[_, readInt -> p2]]) =
  let st = M.singleton (Game (0, p1, 0, p2)) 1
      done m =
        case M.lookupMin m of
          Just (Game _, _) -> False
          _ -> True
      final = until done doRoll' st
      pr1 = M.findWithDefault 0 P1 final
      pr2 = M.findWithDefault 0 P2 final
   in showBS $ max pr1 pr2

data Key
  = Game (Int, Int, Int, Int)
  | P1
  | P2
  deriving (Eq, Ord)

dieFreq :: [(Int, Int)]
dieFreq = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

doRoll' :: M.Map Key Int -> M.Map Key Int
doRoll' m = M.fromListWith (+) $ do
  (key, times) <- M.toList m
  case key of
    Game (score1, pos1, score2, pos2) -> do
      (d1, freq1) <- dieFreq
      let newPos1 = mod10 $ pos1 + d1
          newScore1 = score1 + newPos1
      if newScore1 >= 21
         then [(P1, times * freq1)]
         else do
           (d2, freq2) <- dieFreq
           let newPos2 = mod10 $ pos2 + d2
               newScore2 = score2 + newPos2
           if newScore2 >= 21
              then [(P2, times * freq1 * freq2)]
              else [(Game (newScore1, newPos1, newScore2, newPos2)
                    , times * freq1 * freq2)]
    _ -> [(key, times)]
