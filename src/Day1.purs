module Day1 (run) where

import Prelude

import Data.Filterable as Data.Filterable
import Data.Foldable (foldl)
import Data.Int as Data.Int
import Data.Maybe (Maybe)
import Data.String.Utils as Data.String.Utils
import Effect (Effect)
import Effect.Console (log)
import Util as Util

run :: Effect Unit
run = do
  input <- Util.readInput "day1"
  log "Day 1"
  log ("  Total fuel: " <> show (totalFuel part1 input))
  log ("  Total fuel (compound): " <> show (totalFuel part2 input))

totalFuel :: (Int -> Int) -> String -> Int
totalFuel calculateFuel input = sum fuel
  where
  sum :: Array Int -> Int
  sum = foldl (+) 0

  fuel :: Array Int
  fuel = Data.Filterable.filterMap stringToFuel lines

  stringToFuel :: String -> Maybe Int
  stringToFuel str = map calculateFuel (Data.Int.fromString str)

  lines :: Array String
  lines = Data.String.Utils.lines input

part1 :: Int -> Int
part1 module' = (module' / 3) - 2

part2 :: Int -> Int
part2 module' = case part1 module' of
  fuel' | fuel' > 0 -> fuel' + part2 fuel'
        | otherwise -> 0
