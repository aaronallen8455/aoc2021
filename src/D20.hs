module D20
  ( day20A
  , day20B
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Foldable
import qualified Data.Map.Strict as M
import           Numeric
import           Data.Ix

import           Util

day20A :: BS.ByteString -> BS.ByteString
day20A (break (== "") . BS.lines -> ([decode], _:lns)) =
  let grid = M.fromList $
        zip (range ((0,0), (length lns -1, pred . BS.length $ head lns)))
            (concatMap BS.unpack lns)
      hChar = BS.head decode
      lChar = if hChar == '#' then BS.last decode else '.'
   in showBS . M.size . M.filter (== '#')
    . transform hChar decode
    $ transform lChar decode grid

transform :: Char -> BS.ByteString -> M.Map (Int, Int) Char -> M.Map (Int, Int) Char
transform d decode m =
  M.mapWithKey go (expand d m) where
    go (r, c) _ =
      let inds = do r' <- [r-1..r+1]
                    c' <- [c-1..c+1]
                    pure (r', c')
          vals = map (\x -> M.findWithDefault d x m) inds
          ind = toBin vals
       in BS.index decode ind

expand :: Char -> M.Map (Int, Int) Char -> M.Map (Int, Int) Char
expand d m =
  let ((mnR, mnC), _) = M.findMin m
      ((mxR, mxC), _) = M.findMax m
      sides = (,) <$> [mnR - 1 .. mxR + 1] <*> [mnC-1, mxC + 1]
      tops = (,) <$> [mnR - 1, mxR + 1] <*> [mnC - 1 .. mxC + 1]
   in foldl' go m (sides ++ tops)
  where
    go acc k = M.insert k d acc

toBin :: String -> Int
toBin = fst . head . readBin . map go where
  go '.' = '0'
  go '#' = '1'

day20B :: BS.ByteString -> BS.ByteString
day20B (break (== "") . BS.lines -> ([decode], _:lns)) =
  let grid = M.fromList $
        zip (range ((0,0), (length lns -1, pred . BS.length $ head lns)))
            (concatMap BS.unpack lns)
      hChar = BS.head decode
      lChar = if hChar == '#' then BS.last decode else '.'
   in showBS . M.size . M.filter (== '#') . (!! 25)
      $ iterate ( transform hChar decode
                . transform lChar decode)
                grid
