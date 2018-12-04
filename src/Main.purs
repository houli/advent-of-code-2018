module Main where

import Prelude

import Day1 as Day1
import Day2 as Day2
import Day3 as Day3
import Day4 as Day4
import Effect (Effect)

main :: Effect Unit
main = do
  Day1.main
  Day2.main
  Day3.main
  Day4.main
