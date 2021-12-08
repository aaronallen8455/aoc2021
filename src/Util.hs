module Util
  ( readInt
  , chunksOf
  , biDirectionalBFS
  ) where

import           Control.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import           Data.Maybe

readInt :: BS.ByteString -> Int
readInt = go . BS.readInt where
  go (Just (n, _)) = n
  go _ = error "failed to parse Int"

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ []  = []
chunksOf n (splitAt n -> (h, t)) = h : chunksOf n t

biDirectionalBFS
  :: (Ord k, Semigroup v)
  => (k -> v -> [(k, v)])
  -> (k, v)
  -> (k, v)
  -> Maybe (Int, v)
biDirectionalBFS next start@(startK, startV) end@(endK, endV) =
  go [start] [] (M.singleton startK (0, startV)) [end] [] (M.singleton endK (0, endV)) 0
  where
    go [] [] _ [] [] _ _ = Nothing
    go [] sa sv [] ea ev i =
      go (concat sa) [] sv (concat ea) [] ev $! i + 1
    go (s:sq) sa sv eq ea ev i
        = findMatch s ev i
      <|> go sq (sNxt : sa) sv' eq ea ev i
      where
        sNxt = filter (\(k, _) -> not $ M.member k sv) $ uncurry next s
        sv' = M.insert (fst s) (i, snd s) sv
    go [] sa sv (e:eq) ea ev i
        = findMatch e sv i
      <|> go [] sa sv eq (eNxt : ea) ev' i
      where
        eNxt = filter (\(k, _) -> not $ M.member k ev) $ uncurry next e
        ev' = M.insert (fst e) (i, snd e) ev
    findMatch (k, v) visited i = do
      (d, v2) <- visited M.!? k
      Just (d + i, v <> v2)
