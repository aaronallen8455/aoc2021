module D5
  ( day5A
  , day5B
  , lineToPts
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Tuple
import qualified Data.Map.Strict as M

import           Util

day5A :: BS.ByteString -> BS.ByteString
day5A (map orderPt . filter notDiagonal . map parseLine . BS.lines -> lns)
  = BS.pack . show . M.size . M.filter (> 1)
  . M.fromListWith (+) . map (,1)
  $ concatMap lineToPts lns

type Pt = (Int, Int)
type Line = (Pt, Pt)

notDiagonal :: Line -> Bool
notDiagonal ((x1, y1), (x2, y2))
  = x1 == x2 || y1 == y2

orderPt :: Line -> Line
orderPt pt@((x1, y1), (x2, y2))
  | x1 == x2 = if y1 <= y2
                  then pt
                  else swap pt
  | otherwise = if x1 <= x2
                   then pt
                   else swap pt

lineToPts :: Line -> [Pt]
lineToPts pt@((x1, y1), (x2, y2))
  | x1 == x2 = map (x1,) [y1 .. y2]
  | y1 == y2 = map (,y1) [x1 .. x2]
  | y1 <= y2 = zip [x1 .. x2] [y1 .. y2]
  | otherwise = zip [x1 .. x2] [y1, y1 - 1 .. y2]

parseLine :: BS.ByteString -> Line
parseLine bs
  | ( map readInt . BS.split ',' -> [startX, startY]
    , map readInt . BS.split ',' . BS.drop 3 -> [endX, endY]
    )
  <- BS.break (== '-') bs
    = ((startX, startY), (endX, endY))
  | otherwise = error "bad input"

day5B :: BS.ByteString -> BS.ByteString
day5B (map (orderPt . parseLine) . BS.lines -> lns)
  = BS.pack . show . M.size . M.filter (> 1)
  . M.fromListWith (+) . map (,1)
  $ concatMap lineToPts lns
