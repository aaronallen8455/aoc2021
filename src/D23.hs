module D23
  ( day23A
  , progressMoves
  , test
  ) where

import           Control.Monad.State.Strict
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import           Data.Foldable
import           Data.Traversable
import qualified Data.IntMap.Strict as IM

import           Util
import qualified Data.Vector as V

import           Debug.Trace
import           System.IO.Unsafe

day23A :: BS.ByteString -> BS.ByteString
day23A (BS.transpose . BS.lines -> lns) =
  let toRoom b = (Just $ BS.index b 2, Just $ BS.index b 3)
      room1 = toRoom $ lns !! 3
      room2 = toRoom $ lns !! 5
      room3 = toRoom $ lns !! 7
      room4 = toRoom $ lns !! 9
      st = St room1 room2 room3 room4 (IM.fromList $ [0..10] `zip` repeat Nothing)
   in case (`evalState` DP Nothing mempty) . step $ IM.singleton 0 [Path st 0] of
        Just r -> showBS r
        _ -> error "wtf"

data St =
  St { room1 :: (Maybe Char, Maybe Char)
     , room2 :: (Maybe Char, Maybe Char)
     , room3 :: (Maybe Char, Maybe Char)
     , room4 :: (Maybe Char, Maybe Char)
     , hall :: IM.IntMap (Maybe Char)
     } deriving (Eq, Ord, Show)

data Room = R1 | R2 | R3 | R4 deriving (Bounded, Enum, Eq, Show)
data RoomPos = FirstRoom | SecRoom
data Dir = L | R

atRoom :: Room -> RoomPos -> St -> Maybe Char
atRoom r rp st =
  let pos = case rp of
              FirstRoom -> fst
              SecRoom -> snd
   in case r of
        R1 -> pos $ room1 st
        R2 -> pos $ room2 st
        R3 -> pos $ room3 st
        R4 -> pos $ room4 st

getDest :: Char -> Room
getDest 'A' = R1
getDest 'B' = R2
getDest 'C' = R3
getDest 'D' = R4

-- can approach as a planning problem. The B in first room must go to room 2
-- but before that can happen both C and D must evacuate
-- There is a split on which one acts first and we should prioritize the
-- cheapest action.
-- Which way should a fish go to get out of the way of another will be determined
-- by its expense.

-- each action will be to either:
-- move from a room into another room
-- move from room to hallway (would do this to get out of the way)
-- move from hallway to room (terminal)

-- when moving into the hallway, can record the direction they turned so that
-- it can be moved additional spaces if needed (should not go back the other
-- way unless going to room). Possibly wrong if something went past before second move.

-- the key to cheapest plan is not moving excessively in the hallway?

-- only gets out of the way if it's already in its home, otherwise should be
-- making progress towards home?

-- first thing will be to form a gameplan which will initial be a list of moves
-- where each fish goes to its destination. We don't necessarily know what the
-- destination is because it could be in the back or front of the room.
-- So the move is to just go to a specific room and position doesn't matter?
-- Seems like we need to try all possible orderings of the initial plan to
-- find the cheapest one.

-- will need to have a state that associates game positions with cost so paths
-- that hit a visited state at higher cost can be pruned

-- The frontier of plans will be in a priority queue so that the cheapest one
-- is always tried first.

type Visited = M.Map St Int

data Loc
  = Rm Room
  | Hall Int
  deriving Show

-- data Move =
--   ChangeRoom Char Room Room
--   GoToHall Char Room Int
--   HallToRoom Int Room

-- type Plan = [Move]

type Cost = Int

data Path = Path { pathSt :: !St, pathCost :: !Cost } deriving Show

type Frontier = IM.IntMap [Path]

-- moveCost :: Move -> Int
-- moveCost = undefined

popQueue :: IM.IntMap [Path] -> Maybe (Path, IM.IntMap [Path])
popQueue m = do
  (cost, p:ps) <- IM.lookupMin m
  if null ps
     then Just (p, IM.delete cost m)
     else Just (p, IM.insert cost ps m)

step :: IM.IntMap [Path] -> M (Maybe Int)
step m = do
  DP mDpCost visited <- get
  case popQueue m of
    Just (p, m')
      | maybe False (pathCost p >=) mDpCost -> step m'
      | isDone p -> do
          put $ DP (Just $ pathCost p) visited
          step m'
      | Just c <- M.lookup (pathSt p) visited, c <= pathCost p -> step m'
      | otherwise -> do
          put $ DP mDpCost (M.insert (pathSt p) (pathCost p) visited)
          let newPaths = progressMoves p
              im = IM.fromListWith (<>) $ do
                     p <- newPaths
                     [(pathCost p, [p])]
          step $ IM.unionWith (<>) m' im
    Nothing -> gets curMinCost

isDone :: Path -> Bool
isDone p =
  all roomDone [minBound..]
    where
      roomDone rm =
        case getRoom rm $ pathSt p of
          (Just a, Just b) -> a == b && getDest a == rm
          _ -> False

test :: Path
test =
  Path testSt 0 where
    testSt =
      St { room1 = (Just 'B', Just 'A')
         , room2 = (Just 'C', Just 'D')
         , room3 = (Nothing, Just 'C')
         , room4 = (Just 'D', Just 'A')
         , hall = IM.insert 3 (Just 'B') $ IM.fromList (zip [0..10] $ repeat Nothing)
         }

-- returns a list of desirable moves
progressMoves :: Path -> [Path]
progressMoves p =
  let rooms = [minBound..]
      rms = concatMap (progressRoom p) rooms
      halls = concatMap (progressHall p) hallIx
   in rms ++ halls

hallIx :: [Int]
hallIx = [0,1,3,5,7,9,10]

progressRoom :: Path -> Room -> [Path]
progressRoom p@(Path st cost) rm =
  case getRoom rm st of
    (Just a, Just o)
      | getDest a == rm ->
        if a == o then [] else evacRoom a p rm

      | otherwise -> do
        let dest = getDest a
        p' <- evacRoom a p dest
        Just m <- pure $ numMoves (pathSt p') (Rm rm) (Rm dest)
        let ucost = costToMove a * m
            st' = setRoom (Nothing, Just o)
                    (moveToRoom a dest $ pathSt p')
                    rm
            newCost = pathCost p' + ucost
            newPath = Path st' newCost
        pure newPath
    (Nothing, Just a)
      | getDest a == rm -> []
      | otherwise -> do
        let dest = getDest a
        p' <- evacRoom a p dest
        Just m <- pure $ numMoves (pathSt p') (Rm rm) (Rm dest)
        let ucost = costToMove a * m
            st' = setRoom (Nothing, Nothing)
                    (moveToRoom a dest $ pathSt p')
                    rm
            newCost = pathCost p' + ucost
            newPath = Path st' newCost
        pure newPath

    _ -> []

moveToRoom :: Char -> Room -> St -> St
moveToRoom c rm st =
  case getRoom rm st of
    (Nothing, Nothing) ->
      setRoom (Nothing, Just c) st rm
    (Nothing, o) ->
      setRoom (Just c, o) st rm
    _ -> error "room full"

progressHall :: Path -> Int -> [Path]
progressHall (Path st cost) ix = do
  Just c <- pure $ hall st IM.! ix
  let dest = getDest c
  Just m <- pure $ numMoves st (Hall ix) (Rm dest)
  let newCost = costToMove c * m + cost
      st' = (moveToRoom c dest st)
              { hall = IM.insert ix Nothing $ hall st }
  pure $ Path st' newCost

evacRoom :: Char -> Path -> Room -> [Path]
evacRoom a p@(Path st cost) rm =
  case getRoom rm st of
    (Just c, o) -> do
      hi <- hallIx
      Just m <- pure $ numMoves st (Rm rm) (Hall hi)
      let ucost = costToMove c * m
          st' = (setRoom (Nothing, o) st rm)
            { hall = IM.insert hi (Just c) $ hall st }
          newPath = Path st' (cost + ucost)
      evacRoom a newPath rm
    (Nothing, Just c)
      | c /= a -> do
        hi <- hallIx
        Just m <- pure $ numMoves st (Rm rm) (Hall hi)
        let ucost = costToMove c * m
            st' = (setRoom (Nothing, Nothing) st rm)
              { hall = IM.insert hi (Just c) $ hall st }
            newPath = Path st' (cost + ucost)
        pure newPath
    _ -> [p]

numMoves :: St -> Loc -> Loc -> Maybe Int
numMoves st (Rm r) dest = do
  let next = numMoves st (Hall $ roomToHall r) dest
  case getRoom r st of
    (Just _, _) -> (+ 1) <$> next
    (_, Just _) -> (+ 2) <$> next
    _ -> Nothing
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
            (Just _, _) -> Nothing
            (_, Just c) -> do
              guard $ getDest c == r
              Just 1
            _ -> Just 2
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

getRoom :: Room -> St -> (Maybe Char, Maybe Char)
getRoom = \case
  R1 -> room1
  R2 -> room2
  R3 -> room3
  R4 -> room4

setRoom :: (Maybe Char, Maybe Char) -> St -> Room -> St
setRoom new st = \case
  R1 -> st { room1 = new }
  R2 -> st { room2 = new }
  R3 -> st { room3 = new }
  R4 -> st { room4 = new }

data DP =
  DP { curMinCost :: !(Maybe Cost), visited :: !Visited }

type M a = State DP a

-- solve :: St -> M Int
-- solve st
--   | null pMoves = pure 0
--   | otherwise = search [] [] (map (st,) pMoves)
--   where
--     pMoves = progressMoves st
