module Day4 (main) where

import Prelude

import Common (adventMain, readLines)
import Effect (Effect)
import Effect.Console (log)

getInput :: Effect (Array String)
getInput = readLines "input/day4.txt"

partOne :: Effect Unit
partOne = do
  input <- getInput
  log "hello"

partTwo :: Effect Unit
partTwo = do
  input <- getInput
  log "hello"

main :: Effect Unit
main = do
  adventMain 4 partOne partTwo
