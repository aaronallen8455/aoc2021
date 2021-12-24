{-# LANGUAGE ImplicitParams #-}
module D23
  ( day23A
  , day23B
  ) where

import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

import           Util

day23A :: BS.ByteString -> BS.ByteString
day23A (BS.transpose . BS.lines -> lns) = do
  let toRoom b = [BS.index b 2, BS.index b 3]
      room1 = toRoom $ lns !! 3
      room2 = toRoom $ lns !! 5
      room3 = toRoom $ lns !! 7
      room4 = toRoom $ lns !! 9
      st = St room1 room2 room3 room4 (IM.fromList $ [0..10] `zip` repeat Nothing)
  let ?numCells = 2
   in case (`evalState` DP Nothing mempty) . solve $ IM.singleton 0 [Path st 0] of
        Just r -> showBS r
        _ -> error "wtf"

day23B :: BS.ByteString -> BS.ByteString
day23B (BS.transpose . BS.lines -> lns) = do
  let toRoom x y b = [BS.index b 2, x, y, BS.index b 3]
      room1 = toRoom 'D' 'D' $ lns !! 3
      room2 = toRoom 'C' 'B' $ lns !! 5
      room3 = toRoom 'B' 'A' $ lns !! 7
      room4 = toRoom 'A' 'C' $ lns !! 9
      st = St room1 room2 room3 room4 (IM.fromList $ [0..10] `zip` repeat Nothing)
  let ?numCells = 4
   in case (`evalState` DP Nothing mempty) . solve $ IM.singleton 0 [Path st 0] of
        Just r -> showBS r
        _ -> error "wtf"

data St =
  St { room1 :: [Char]
     , room2 :: [Char]
     , room3 :: [Char]
     , room4 :: [Char]
     , hall :: IM.IntMap (Maybe Char)
     } deriving (Eq, Ord, Show)

data Room = R1 | R2 | R3 | R4 deriving (Bounded, Enum, Eq, Show)

getDest :: Char -> Room
getDest 'A' = R1
getDest 'B' = R2
getDest 'C' = R3
getDest 'D' = R4

type Visited = M.Map St Int

data Loc
  = Rm Room
  | Hall Int
  deriving Show

type Cost = Int

data Path = Path { pathSt :: !St, pathCost :: !Cost } deriving Show

popQueue :: IM.IntMap [Path] -> Maybe (Path, IM.IntMap [Path])
popQueue m = do
  (cost, p:ps) <- IM.lookupMin m
  if null ps
     then Just (p, IM.delete cost m)
     else Just (p, IM.insert cost ps m)

solve :: (?numCells :: Int) => IM.IntMap [Path] -> M (Maybe Int)
solve m = do
  DP mDpCost visited <- get
  case popQueue m of
    Just (p, m')
      | maybe False (pathCost p >=) mDpCost -> solve m'
      | isDone p -> do
          put $ DP (Just $ pathCost p) visited
          solve m'
      | Just c <- M.lookup (pathSt p) visited, c <= pathCost p -> solve m'
      | otherwise -> do
          put $ DP mDpCost (M.insert (pathSt p) (pathCost p) visited)
          let newPaths = progressMoves p
              im = IM.fromListWith (<>) $ do
                     p <- newPaths
                     [(pathCost p, [p])]
          solve $ IM.unionWith (<>) m' im
    Nothing -> gets curMinCost

isDone :: (?numCells :: Int) => Path -> Bool
isDone p =
  all roomDone [minBound..]
    where
      roomDone rm =
        let rs = getRoom rm $ pathSt p
         in all (\x -> getDest x == rm) rs
            && length rs == ?numCells

progressMoves :: (?numCells :: Int) => Path -> [Path]
progressMoves p =
  let rooms = [minBound..]
      rms = concatMap (progressRoom p) rooms
      halls = concatMap (progressHall p) hallIx
   in rms ++ halls

hallIx :: [Int]
hallIx = [0,1,3,5,7,9,10]

progressRoom :: (?numCells :: Int) => Path -> Room -> [Path]
progressRoom p@(Path st cost) rm =
  case getRoom rm st of
    xs | all (\x -> getDest x == rm) xs -> []
    (c:rest) -> do
      ix <- hallIx
      Just m <- pure $ numMoves st (Rm rm) (Hall ix)
      let ucost = costToMove c * m
          st' = setRoom rest
                  (setHall ix c $ pathSt p)
                  rm
          newCost = pathCost p + ucost
          newPath = Path st' newCost
      pure newPath

setHall :: Int -> Char -> St -> St
setHall ix c st =
  st { hall = IM.insert ix (Just c) $ hall st }

moveToRoom :: Char -> Room -> St -> St
moveToRoom c rm st =
  case getRoom rm st of
    rs ->
      setRoom (c : rs) st rm

progressHall :: (?numCells :: Int) => Path -> Int -> [Path]
progressHall (Path st cost) ix = do
  Just c <- pure $ hall st IM.! ix
  let dest = getDest c
  Just m <- pure $ numMoves st (Hall ix) (Rm dest)
  let newCost = costToMove c * m + cost
      st' = (moveToRoom c dest st)
              { hall = IM.insert ix Nothing $ hall st }
  pure $ Path st' newCost

numMoves :: (?numCells :: Int) => St -> Loc -> Loc -> Maybe Int
numMoves st (Rm r) dest = do
  let next = numMoves st (Hall $ roomToHall r) dest
  case getRoom r st of
    [] -> Nothing
    xs -> (+ (?numCells - length xs + 1)) <$> next
numMoves st (Hall a) (Hall b) =
  case compare a b of
    LT | Nothing <- hall st IM.! (a+1)
      -> succ <$> numMoves st (Hall $ a + 1) (Hall b)
    GT | Nothing <- hall st IM.! (a-1)
      -> succ <$> numMoves st (Hall $ a - 1) (Hall b)
    EQ -> Just 0
    _ -> Nothing
numMoves st (Hall i) (Rm r) =
  if roomToHall r == i
     then case getRoom r st of
            xs | length xs == ?numCells -> Nothing
               | all (\x -> getDest x == r) xs ->
                   Just $ ?numCells - length xs
               | otherwise -> Nothing
     else do
       x <- numMoves st (Hall i) (Hall $ roomToHall r)
       y <- numMoves st (Hall $ roomToHall r) (Rm r)
       pure $ x + y

costToMove :: Char -> Cost
costToMove = \case
  'A' -> 1
  'B' -> 10
  'C' -> 100
  'D' -> 1000

roomToHall :: Room -> Int
roomToHall R1 = 2
roomToHall R2 = 4
roomToHall R3 = 6
roomToHall R4 = 8

getRoom :: Room -> St -> [Char]
getRoom = \case
  R1 -> room1
  R2 -> room2
  R3 -> room3
  R4 -> room4

setRoom :: [Char] -> St -> Room -> St
setRoom new st = \case
  R1 -> st { room1 = new }
  R2 -> st { room2 = new }
  R3 -> st { room3 = new }
  R4 -> st { room4 = new }

data DP =
  DP { curMinCost :: !(Maybe Cost), visited :: !Visited }

type M a = State DP a

