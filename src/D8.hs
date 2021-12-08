module D8
  ( day8A
  , day8B
  ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Tuple
import           Data.Maybe

import           Debug.Trace

day8A :: BS.ByteString -> BS.ByteString
day8A (map (BS.words . BS.tail . snd . BS.break (== '|')) . BS.lines -> inp) =
  BS.pack . show . sum $ map (length . filter uniqueDigit) inp

uniqueDigit :: BS.ByteString -> Bool
uniqueDigit bs =
  case BS.length bs of
    2 -> True
    4 -> True
    3 -> True
    7 -> True
    _ -> False

day8B :: BS.ByteString -> BS.ByteString
day8B (map ((\(x,y) -> (BS.words x, BS.words $ BS.tail y)) . BS.break (== '|')) . BS.lines -> inp) =
  BS.pack . show . sum $ map (\(i, o) -> read $ concatMap (fromJust . solve (findConnections i)) o) inp
--  BS.pack . show . sum $ map (length . filter uniqueDigit . traceShowId) inp

solve :: M.Map (S.Set Char) Int -> BS.ByteString -> Maybe String
solve m o = show <$> m M.!? S.fromList (BS.unpack o)
  --matchDigit . S.fromList . mapMaybe (`M.lookup` m) $ BS.unpack o

matchDigit :: S.Set Char -> Maybe Int
matchDigit s =
  case S.toList s of
    "abcefg" -> Just 0
    "cf" -> Just 1
    "acdeg" -> Just 2
    "acdfg" -> Just 3
    "bcdf" -> Just 4
    "abdfg" -> Just 5
    "abdefg" -> Just 6
    "acf" -> Just 7
    "abcdefg" -> Just 8
    "abcdfg" -> Just 9
    _ -> Nothing

-- findConnections :: [BS.ByteString] -> [BS.ByteString] -> M.Map Char Char
-- findConnections output
--   = traceShowId
--   . revMap
--   . fromJust
--   . matching
--   . foldl' go mempty where
-- 
-- 
--   --reduce = fmap (head . S.toList)
-- --   combine :: Char -> S.Set Char -> S.Set Char -> S.Set Char
-- --   combine c sets1 sets2 = filter (not . null) $ do
-- --     s1 <- sets1
-- --     s2 <- sets2
-- --     [S.intersection s1 s2]
-- 
--   matching :: M.Map Char (S.Set Char) -> Maybe (M.Map Char Char)
--   matching m =
--     foldM (\acc x -> assign m x mempty acc) mempty (M.keys m)
-- 
--   checkAgainstOutput :: M.Map Char Char -> Bool
--   checkAgainstOutput (revMap -> m) = and $ do
--     o <- BS.unpack <$> output
--     pure $ case traverse (`M.lookup` m) o of
--       Nothing -> True
--       Just cs -> traceShow cs $!
--         case matchDigit (S.fromList cs) of
--           Nothing -> False
--           _ -> True
-- 
--   assign :: M.Map Char (S.Set Char)
--          -> Char
--          -> S.Set Char
--          -> M.Map Char Char -- reversed mapping
--          -> Maybe (M.Map Char Char)
--   assign possible c visited assigned
--     | S.member c visited = Nothing
--     | otherwise = asum $ do
--         pos <- foldMap toList $ M.lookup c possible
-- 
--         let newAssigned = M.insert pos c assigned
-- 
--         case M.lookup pos assigned of
--           Nothing -> do
--             guard $ checkAgainstOutput newAssigned
--             [Just newAssigned]
--           Just newC -> do
--             let newVisited = S.insert c visited
--             Just res <- [assign possible newC newVisited newAssigned]
--             guard $ checkAgainstOutput res
--             [Just res]

-- What if the mapping was from number to list of Sets?
-- then narrowing down would be finding combos where shared
-- characters match

findConnections :: [BS.ByteString] -> M.Map (S.Set Char) Int
findConnections
  = -- traceShowId
   revMap
  . fromJust
  . matching
  . traceShowId
  . foldl' go mempty
  where

  revMap = M.fromList . map swap .  M.toList

  matching :: M.Map (S.Set Char) (S.Set Int) -> Maybe (M.Map Int (S.Set Char))
  matching m =
    foldM (\acc x -> assign m x mempty acc) mempty (M.keys m)

  assign :: M.Map (S.Set Char) (S.Set Int)
         -> S.Set Char
         -> S.Set (S.Set Char)
         -> M.Map Int (S.Set Char) -- reversed mapping
         -> Maybe (M.Map Int (S.Set Char))
  assign possible str visited assigned
    | S.member str visited = Nothing
    | otherwise = asum $ do
        pos <- foldMap toList $ M.lookup str possible

        let newAssigned = M.insert pos str assigned

        case M.lookup pos assigned of
          Nothing -> do
            [Just newAssigned]
          Just newC -> do
            let newVisited = S.insert str visited
            [assign possible newC newVisited newAssigned]

  go m (S.fromList . BS.unpack -> bs)
    = M.unionWith (<>) m . M.fromList $
    case length bs of
      -- 1
      2 -> do
        [(bs, S.singleton 1)]
      4 -> do
        [(bs, S.singleton 4)]
      3 -> do
        [(bs, S.singleton 7)]
      7 ->
        [(bs, S.singleton 8)]
      5 ->
        [(bs, S.fromList [2, 3, 5])]
      6 ->
        [(bs, S.fromList [6, 9, 0])]
      _ -> []
