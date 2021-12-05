module Main where

import qualified Data.ByteString as BS
import           System.Environment (getArgs)

import           D1
import           D2
import           D3
import           D4
import           D5

main :: IO ()
main = do
  day:c:_ <- getArgs

  BS.interact $ case day <> c of
    "1a" -> day1A
    "1b" -> day1B
    "2a" -> day2A
    "2b" -> day2B
    "3a" -> day3A
    "3b" -> day3B
    "4a" -> day4A
    "4b" -> day4B
    "5a" -> day5A
    "5b" -> day5B

  putStrLn ""
  putStrLn "--------------------------------------------------------------------------------"
