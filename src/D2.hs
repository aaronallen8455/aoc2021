module D2
  ( day2A
  , day2B
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List

import           Util

day2A :: BS.ByteString -> BS.ByteString
day2A = BS.pack . show
      . uncurry (*)
      . List.foldl' go (0, 0)
      . map parseDirection
      . BS.lines
  where
    go :: (Int, Int) -> Either Int Int -> (Int, Int)
    go (h, d) = \case
      Left n -> (h + n, d)
      Right n -> (h, d + n)

parseDirection :: BS.ByteString -> Either Int Int
parseDirection bs
  | ("forward ", n) <- BS.splitAt 8 bs = Left $ readInt n
  | ("down ", n) <- BS.splitAt 5 bs = Right $ readInt n
  | ("up ", n) <- BS.splitAt 3 bs = Right . negate $ readInt n

day2B :: BS.ByteString -> BS.ByteString
day2B = BS.pack . show
      . (\(h,d,_) -> h * d)
      . List.foldl' go (0, 0, 0)
      . map parseDirection
      . BS.lines
  where
    go (h, d, a) = \case
      Left n -> (h + n, d + a * n, a)
      Right n -> (h, d, a + n)
