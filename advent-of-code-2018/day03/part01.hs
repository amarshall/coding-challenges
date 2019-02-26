#!/usr/bin/env stack
-- stack --resolver lts-13.8 script --package regex-tdfa --package containers
{-# OPTIONS_GHC -threaded -rtsopts -with-rtsopts=-N -Wall -fno-warn-name-shadowing #-}

import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Text.Regex.TDFA

type Grid = Map (Int, Int) Int

data Piece = Piece { id :: Int, x1 :: Int, y1 :: Int, x2 :: Int, y2 :: Int }
  deriving (Eq, Show)

instance Ord Piece where
  compare (Piece {x1=ax, y1=ay}) (Piece {x1=bx, y1=by}) = compare [ax, ay] [bx, by]

parse :: String -> Maybe Piece
parse str =
  if null result then Nothing
  else return $ mkPiece vals
  where
    result = str =~ "^#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)$" :: [[String]]
    vals = result & head & drop 1 & map (\s -> read s :: Int)
    mkPiece [a,b,c,d,e] = Piece a b c (b + d - 1) (c + e - 1)

area :: Piece -> Int
area Piece {x1=x1, y1=y1, x2=x2, y2=y2} = (x2 - x1) * (y2 - y1)

isValid :: Piece -> Bool
isValid Piece {x1=x1, y1=y1, x2=x2, y2=y2} = (x2 > x1) && (y2 > y1)

points :: Piece -> [(Int, Int)]
points piece = [(x, y) | x <- [x1..x2], y <- [y1..y2]]
  where
    Piece {x1=x1, y1=y1, x2=x2, y2=y2} = piece

mkGrid :: [Piece] -> Grid
mkGrid pieces = Map.fromListWith (+) kvs
  where
    pts = concat $ map points pieces
    kvs = zip pts (repeat 1)

totalOverlap :: Grid -> Int
totalOverlap = Map.size . Map.filter ((<) 1)

main :: IO ()
main = do
  input <- getContents
  input
    & lines
    & map parse
    & catMaybes
    & mkGrid
    & totalOverlap
    & show
    & putStrLn
