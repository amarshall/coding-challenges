#!/usr/bin/env stack
-- stack --resolver lts-13.8 script --ghc-options -Wall --ghc-options -Werror --ghc-options -threaded --ghc-options -rtsopts --ghc-options -with-rtsopts=-N --package containers

import Data.Function
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

counts :: Ord a => [a] -> Map a Int
counts xs = Map.fromListWith (+) (zip xs (repeat 1))

hasDupsOf :: Int -> String -> Bool
hasDupsOf n s = s & counts & Map.filter ((==) n) & Map.null & not

numDupsOf :: Int -> [String] -> Int
numDupsOf n = length . filter (hasDupsOf n)

checksum :: [String] -> Int
checksum ss = (numDupsOf 2 ss) * (numDupsOf 3 ss)

main :: IO ()
main = do
  input <- getContents
  input
    & lines
    & checksum
    & show
    & putStrLn
