module Main where

import qualified Data.ByteString as BS
import           System.Environment (getArgs)

import           D1
import           D2

main :: IO ()
main = do
  day:c:_ <- getArgs

  BS.interact $ case day <> c of
    "1a" -> day1A
    "1b" -> day1B
    "2a" -> day2A
    "2b" -> day2B

  putStrLn ""
  putStrLn "--------------------------------------------------------------------------------"
