module Common
  ( adventMain
  , countsMap
  , readLines
  ) where

import Prelude

import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

adventMain :: Int -> Effect Unit -> Effect Unit -> Effect Unit
adventMain day partOne partTwo = do
  log $ "DAY " <> show day
  log "Part 1:"
  partOne
  log "Part 2:"
  partTwo
  log "-------------------------"

readLines :: String -> Effect (Array String)
readLines filename = do
  contents <- readTextFile UTF8 filename
  pure $ split (Pattern "\n") contents

countsMap :: forall a. Ord a => List a -> Map a Int
countsMap = countsMap' Map.empty
 where
   countsMap' :: Ord a => Map a Int -> List a -> Map a Int
   countsMap' map Nil = map
   countsMap' map (Cons x xs) = countsMap' (Map.alter countsHelper x map) xs

   countsHelper :: Maybe Int -> Maybe Int
   countsHelper Nothing = Just 1
   countsHelper (Just x) = Just (x + 1)
