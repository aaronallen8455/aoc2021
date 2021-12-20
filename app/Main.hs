module Main where

import qualified Data.ByteString as BS
import           System.Environment (getArgs)

import           D1
import           D2
import           D3
import           D4
import           D5
import           D6
import           D7
import           D8
import           D9
import           D10
import           D11
import           D12
import           D13
import           D14
import           D15
import           D16
import           D17
import           D18
import           D19
import           D20

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
    "6a" -> day6A
    "6b" -> day6B
    "7a" -> day7A
    "7b" -> day7B
    "8a" -> day8A
    "8b" -> day8B
    "9a" -> day9A
    "9b" -> day9B
    "10a" -> day10A
    "10b" -> day10B
    "11a" -> day11A
    "11b" -> day11B
    "12a" -> day12A
    "12b" -> day12B
    "13a" -> day13A
    "13b" -> day13B
    "14a" -> day14A
    "14b" -> day14B
    "15a" -> day15A
    "15b" -> day15B
    "16a" -> day16A
    "16b" -> day16B
    "17a" -> day17A
    "17b" -> day17B
    "18a" -> day18A
    "18b" -> day18B
    "19a" -> day19A
    "19b" -> day19B
    "20a" -> day20A
    "20b" -> day20B

  putStrLn ""
  putStrLn "--------------------------------------------------------------------------------"
