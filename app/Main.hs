module Main where

import qualified Data.ByteString as BS
import           System.Environment (getArgs)

import           D1

main :: IO ()
main = do
  day:c:_ <- getArgs

  BS.interact $ case day <> c of
    "1a" -> day1A

  putStrLn ""
  putStrLn "--------------------------------------------------------------------------------"
