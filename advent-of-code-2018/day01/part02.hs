#!/usr/bin/env stack
-- stack --resolver lts-13.8 script --ghc-options -Wall --ghc-options -Werror --ghc-options -threaded --ghc-options -rtsopts --ghc-options -with-rtsopts=-N --package containers

import Data.Function
import qualified Data.Set as Set

parseInt :: String -> Int
parseInt ('+':s) = read s :: Int
parseInt s = read s :: Int

parse :: String -> [Int]
parse = map parseInt . lines

rollingSum :: Num a => [a] -> [a]
rollingSum = scanl (+) 0

-- Like find + until
findFold :: (a -> b -> Bool) -> (a -> b -> b) -> b -> [a] -> Maybe a
findFold _ _ _ [] = Nothing
findFold predicate fn acc (x:xs) =
  if predicate x acc then return x
  else findFold predicate fn (fn x acc) xs

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate = findFold Set.member Set.insert Set.empty

main :: IO ()
main = do
  input <- getContents
  parse input
    & cycle
    & rollingSum
    & firstDuplicate
    & show
    & putStrLn
