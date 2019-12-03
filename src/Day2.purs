module Day2 (run) where

import Prelude

import Data.Array ((!!), (..))
import Data.Array as Data.Array
import Data.Either (Either(..))
import Data.Either as Data.Either
import Data.Filterable as Data.Filterable
import Data.Foldable (foldl)
import Data.Int as Data.Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Data.Maybe
import Data.String as Data.String
import Effect (Effect)
import Effect.Console (log)
import Util as Util

run :: Effect Unit
run = do
  input <- Util.readInput "day2"
  let
    program = (readProgram input)
    inputs = findInputs 19690720 program
  log "Day 2"
  log case inputs of
    Left error -> error
    Right result -> "Inputs: " <> show result

findInputs :: Int -> Array Int -> Either String Int
findInputs answer program = do
  find (Left "Failed at start") \(done :: Either String Int) (noun :: Int) ->
    find done \(done' :: Either String Int) (verb :: Int) -> case done' of
      Right result -> Right result
      Left error -> do
        let
          program' = prepareProgram noun verb program
        case runProgram program' of
          Left error' -> Left error'
          Right output -> do
            if output == answer
            then Right (noun * 100 + verb)
            else Left ("Failed with: " <> show (noun * 100 + verb))
  where
  find :: _
  find d f = foldl f d (0..99)

runProgram :: Array Int -> Either String Int
runProgram program =
  Data.Either.note
    "Unknown Error"
    (go 0 program >>= (\completed -> completed !! 0))
  where
  go :: Int -> Array Int -> Maybe (Array Int)
  go idx program' = do
    opcode <- program' !! idx
    case opcode of
      99 -> Just program'
      opcode'
        | opcode' == 1 || opcode' == 2 -> do
          xIdx <- program' !! (idx + 1)
          yIdx <- program' !! (idx + 2)
          destinationIdx <- program' !! (idx + 3)
          result <- operation opcode' <$> (program' !! xIdx) <*> (program' !! yIdx)
          updated <- Data.Array.updateAt destinationIdx result program'
          go (idx + 4) updated
        | otherwise -> Nothing

readProgram :: String -> Array Int
readProgram str = ints
  where
    ints :: Array Int
    ints = Data.Filterable.filterMap Data.Int.fromString parsed

    parsed :: Array String
    parsed = Data.String.split (Data.String.Pattern ",") str

operation :: Int -> Int -> Int -> Int
operation = case _ of
  1 -> (+)
  2 -> (*)
  code -> const

prepareProgram :: Int -> Int -> Array Int -> Array Int
prepareProgram noun verb input =  Data.Maybe.fromMaybe [] do
    program <- Data.Array.updateAt 1 noun input
    Data.Array.updateAt 2 verb program
