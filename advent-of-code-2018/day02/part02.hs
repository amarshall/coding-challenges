#!/usr/bin/env stack
-- stack --resolver lts-13.8 script --ghc-options -Wall --ghc-options -Werror --ghc-options -threaded --ghc-options -rtsopts --ghc-options -with-rtsopts=-N --package containers

import Data.Function
import Data.List

combinations :: [a] -> [(a, a)]
combinations [] = []
combinations (x:xs) = map ((,) x) xs ++ combinations xs

onlyEq :: Eq a => [a] -> [a] -> [a]
onlyEq xs ys = map fst $ filter (uncurry (==)) $ zip xs ys

numEq :: Eq a => [a] -> [a] -> Int
numEq xs ys = length $ onlyEq xs ys

compareEq :: Eq a => ([a], [a]) -> ([a], [a]) -> Ordering
compareEq x y = (uncurry numEq x) `compare` (uncurry numEq y)

mostEqual :: Eq a => [[a]] -> ([a], [a])
mostEqual xss = maximumBy compareEq $ combinations xss

main :: IO ()
main = do
  input <- getContents
  input
    & lines
    & mostEqual
    & uncurry onlyEq
    & putStrLn
