module Day1 (main) where

import Prelude

import Common (adventMain, readLines)
import Data.Array (catMaybes)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.List.Lazy (List, Step(..), cycle, fromFoldable, step)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Console (logShow)

getInput :: Effect (Array Int)
getInput = do
  lines <- readLines "input/day1.txt"
  pure $ catMaybes $ fromString <$> lines

partOne :: Effect Unit
partOne = do
  input <- getInput
  logShow $ sum input

partTwo :: Effect Unit
partTwo = do
  input <- getInput
  logShow $ firstRepeat $ cycle $ fromFoldable input

firstRepeat :: List Int -> Maybe Int
firstRepeat xs = firstRepeat' Set.empty xs 0
  where
    firstRepeat' :: Set Int -> List Int -> Int -> Maybe Int
    firstRepeat' seen nums current = case step nums of
      Nil -> Nothing
      Cons num nums' ->
        if Set.member current seen
        then
          Just current
        else
          firstRepeat' (Set.insert current seen) nums' (current + num)

main :: Effect Unit
main = do
  adventMain 1 partOne partTwo
