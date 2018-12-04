module Day3 (main) where

import Prelude

import Common (adventMain, countsMap, readLines)
import Data.Array (all, catMaybes, find)
import Data.Either (Either, hush)
import Data.List (List, fromFoldable, (..))
import Data.Map as Map
import Effect (Effect)
import Effect.Console (logShow)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.Token (makeTokenParser)

type Point =
  { x :: Int
  , y :: Int
  }

type Claim =
  { id :: Int
  , origin :: Point
  , width :: Int
  , height :: Int
  }

rights :: forall a b. Array (Either a b) -> Array b
rights = (map hush) >>> catMaybes

claimParser :: Parser String Claim
claimParser = do
  let tokenParser = makeTokenParser emptyDef
  _ <- tokenParser.symbol "#"
  id <- tokenParser.integer
  _ <- tokenParser.symbol "@"
  x <- tokenParser.integer
  _ <- tokenParser.comma
  y <- tokenParser.integer
  _ <- tokenParser.symbol ":"
  width <- tokenParser.integer
  _ <- tokenParser.symbol "x"
  height <- tokenParser.integer
  pure { id, origin: { x, y }, width, height }

getInput :: Effect (Array Claim)
getInput = do
  lines <- readLines "input/day3.txt"
  let claims = rights $ (flip runParser claimParser) <$> lines
  pure claims

partOne :: Effect Unit
partOne = do
  claims <- fromFoldable <$> getInput
  let allPointsForClaims = claims >>= pointsForClaim
  -- Oops this is slow and bad lol
  let countsPoints = countsMap allPointsForClaims
  let overlappingPoints = Map.filter (_ >= 2) countsPoints
  logShow $ Map.size overlappingPoints

pointsForClaim :: Claim -> List Point
pointsForClaim claim = do
  x <- claim.origin.x .. (claim.origin.x + claim.width - 1)
  y <- claim.origin.y .. (claim.origin.y + claim.height - 1)
  pure { x, y }

partTwo :: Effect Unit
partTwo = do
  claims <- getInput
  let found = find (\x -> all (\y -> not $ claimsOverlap x y) claims) claims
  logShow found

claimsOverlap :: Claim -> Claim -> Boolean
claimsOverlap claim1 claim2 =
  claim1.id /= claim2.id
  && claim1.origin.y <= (claim2.origin.y + claim2.height - 1)
  && claim2.origin.y <= (claim1.origin.y + claim1.height - 1)
  && claim1.origin.x <= (claim2.origin.x + claim2.width - 1)
  && claim2.origin.x <= (claim1.origin.x + claim1.width - 1)

main :: Effect Unit
main = do
  adventMain 3 partOne partTwo
