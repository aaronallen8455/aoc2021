module D4
  ( day4A
  , day4B
  ) where

import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans
import           Control.Monad.Trans.Cont
import qualified Data.Array.MArray as A
import           Data.Array.ST (STArray)
import qualified Data.ByteString.Char8 as BS
import           Data.Foldable
import qualified Data.List as L
import           Data.Maybe
import           Data.Traversable

import           Util

day4A :: BS.ByteString -> BS.ByteString
day4A (BS.lines -> nbs:_:boardsBs) =
  let nums = readInt <$> BS.split ',' nbs
      boards = map (map (map readInt . BS.words))
             . filter ((>1) . length)
             $ L.groupBy (\a b -> not (BS.null a) && not (BS.null b)) boardsBs
   in runST $ do
     boardArrs <- traverse mkBoard boards
     BS.pack . show <$> solveA nums boardArrs

type Board s = STArray s (Int, Int) (Maybe Int)

mkBoard :: [[Int]] -> ST s (Board s)
mkBoard = A.newListArray ((0,0),(4,4)) . concatMap (map Just)

solveA :: [Int] -> [Board s] -> ST s Int
solveA nums boards = evalContT $ traverse_ go nums >> error "no solution"
  where
    go n = do
      for_ boards $ \board -> do
        mCoord <- lift $ findNum board n
        isWinner <- lift $ traverse (checkForWin board) mCoord
        when (isWinner == Just True) $ do
          summed <- lift $ sumElems board
          shiftT $ \_ -> pure $ summed * n

checkForWin :: Board s -> (Int, Int) -> ST s Bool
checkForWin board (r, c) = do
  a <- fmap (all (== Nothing)) . for [0..4] $ \col -> do
    A.readArray board (r, col)
  b <- fmap (all (== Nothing)) . for [0..4] $ \row -> do
    A.readArray board (row, c)
  pure $ a || b

sumElems :: Board s -> ST s Int
sumElems = fmap (sum . catMaybes) . A.getElems

findNum :: Board s -> Int -> ST s (Maybe (Int, Int))
findNum board n = evalContT $ do
  for_ [0..4] $ \r ->
    for_ [0..4] $ \c -> do
      x <- lift $ A.readArray board (r, c)
      when (x == Just n) $ do
        lift $ A.writeArray board (r, c) Nothing
        shiftT $ \_ -> pure $ Just (r, c)

  pure Nothing

day4B :: BS.ByteString -> BS.ByteString
day4B (BS.lines -> nbs:_:boardsBs) =
  let nums = readInt <$> BS.split ',' nbs
      boards = map (map (map readInt . BS.words))
             . filter ((>1) . length)
             $ L.groupBy (\a b -> not (BS.null a) && not (BS.null b)) boardsBs
   in runST $ do
     boardArrs <- traverse mkBoard boards
     BS.pack . show <$> solveB nums boardArrs

solveB :: [Int] -> [Board s] -> ST s Int
solveB nums = evalContT . go nums
  where
    go (n:ns) boards = do
      newBoards <- fmap catMaybes . for boards $ \board -> do
        mCoord <- lift $ findNum board n
        isWinner <- lift $ traverse (checkForWin board) mCoord
        if isWinner == Just True
           then do
             when (length boards == 1) $ do
               summed <- lift $ sumElems board
               shiftT $ \_ -> pure $ summed * n
             pure Nothing
           else pure $ Just board

      go ns newBoards
