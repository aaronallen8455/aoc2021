module D19
  ( day19A
  , day19B
  ) where

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S
import qualified Data.List as L
import           Data.Maybe

import           Util

type Coord = (Int, Int, Int) -- X Y Z

day19A :: BS.ByteString -> BS.ByteString
day19A (BS.lines -> lns) =
  let mCoords = fmap (S.fromList . fmap fromJust)
              . filter (isJust . head)
              . L.groupBy (\a b -> isJust a && isJust b) $ parseCoord <$> lns
   in showBS . S.size . fst $ combineAll mCoords

parseCoord :: BS.ByteString -> Maybe Coord
parseCoord (map readInt . BS.split ',' -> [x,y,z]) = Just (x,y,z)
parseCoord _ = Nothing

translation :: Coord -> Coord -> (Coord -> Coord)
translation (x,y,z) (a,b,c) (q,w,e) =
  (q - (a - x), w - (b - y), e - (c - z))

day19B :: BS.ByteString -> BS.ByteString
day19B (BS.lines -> lns) =
  let mCoords = fmap (S.fromList . fmap fromJust)
              . filter (isJust . head)
              . L.groupBy (\a b -> isJust a && isJust b) $ parseCoord <$> lns
   in showBS . findMax . snd $ combineAll mCoords

combineAll :: [S.Set Coord] -> (S.Set Coord, [Coord])
combineAll (s : rest) = fst $ until (null . snd) go ((s, [(0,0,0)]), rest) where
  go ((x, locs), y :xs)
    = case tryMatch x y of
        Nothing -> ((x, locs), xs ++ [y])
        Just (r, d) -> ((r, d:locs), xs)

findMax :: [Coord] -> Int
findMax cs = maximum $ do
  (a:rest) <- L.tails cs
  r <- rest
  let tf = translation a r
  pure $ (\(x,y,z) -> abs x + abs y + abs z) $ tf (0,0,0)

tryMatch :: S.Set Coord -> S.Set Coord -> Maybe (S.Set Coord, Coord)
tryMatch a b = listToMaybe $ do
  perm <- allPerms
  let b' = S.map perm b
  c <- S.toList a
  bc <- S.toList b'
  let tf = translation c bc
  let b'' = S.map tf b'
      inter = S.intersection a b''
  guard $ S.size inter >= 12
  pure (a <> b'', tf (0,0,0))


allPerms :: [Coord -> Coord]
allPerms = do
  f <- facing
  u <- whichUp
  [u . f]

facing :: [Coord -> Coord]
facing =
  [ \(x,y,z) -> (x,y,z)
  , \(x,y,z) -> (y,-x,z) -- turn left
  , \(x,y,z) -> (-x,-y,z)
  , \(x,y,z) -> (-y,x,z)

  , \(x,y,z) -> (x,z,-y)
  , \(x,y,z) -> (x,-z,y)
  ]

whichUp :: [Coord -> Coord]
whichUp =
  [ \(x,y,z) -> (x,y,z)
  , \(x,y,z) -> (z,y,-x)
  , \(x,y,z) -> (-x,y,-z)
  , \(x,y,z) -> (-z,y,x)
  ]
