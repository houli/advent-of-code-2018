module Day2 (main) where

import Prelude

import Common (adventMain, countsMap, readLines)
import Data.Array (filter, head)
import Data.Array as Array
import Data.Foldable (elem, find, length)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Pair (Pair, fst, snd, (~))
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Effect.Console (log, logShow)

getInput :: Effect (Array String)
getInput = readLines "input/day2.txt"

partOne :: Effect Unit
partOne = do
  input <- getInput
  let twos = filter (containsExactly 2) input
  let threes = filter (containsExactly 3) input
  logShow $ length twos * length threes :: Int -- Not sure why this annotation is needed

containsExactly :: Int -> String -> Boolean
containsExactly n str = elem n (Map.values $ countsMap (List.fromFoldable $ toCharArray str))

partTwo :: Effect Unit
partTwo = do
  input <- getInput
  let combinationCharListPairs = (map >>> map) (toCharArray >>> List.fromFoldable) (allCombinations input)
  let commonElems = map (\x -> commonElements (fst x) (snd x)) combinationCharListPairs
  let result = do
       firstInput <- head input
       let fullLength = String.length firstInput
       find (\x -> length x == fullLength - 1) commonElems
  case result of
    Nothing -> log "Yikes"
    Just found -> log $ (Array.fromFoldable >>> fromCharArray) found

allCombinations :: forall a. Array a -> Array (Pair a)
allCombinations xs = do
  x <- xs
  y <- xs
  pure $ x ~ y

commonElements :: forall a. Eq a => List a -> List a -> List a
commonElements Nil _ = Nil
commonElements _ Nil = Nil
commonElements (Cons x xs) (Cons y ys) =
  if x == y
  then
    x : commonElements xs ys
  else
    commonElements xs ys

main :: Effect Unit
main = do
  adventMain 2 partOne partTwo
