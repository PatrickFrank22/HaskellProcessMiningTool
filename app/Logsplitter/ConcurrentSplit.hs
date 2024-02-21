{-
This implementation is based on:
Sander J. J. Leemans. Robust Process Mining with Guarantees - Process Discovery, Conformance Checking and Enhancement, volume 440 of Lecture Notes in Business Information Processing. Springer, 2022. ISBN 978-3-030-96654-6. doi: 10.1007/ 978-3-030-96655-3. https://doi.org/10.1007/978-3-030-96655-3.
-}

module Logsplitter.ConcurrentSplit (
    concurrentSplit
) where

import Helperfunctions.HelperFunctions


concurrentSplit :: [[String]] -> [(Int, [String])] -> [[(Int, [String])]]
concurrentSplit [] _ = []
concurrentSplit (subset:subsets) log = 
  (sumValuesByTraces (concurrentSplitPerSubset subset log)) : concurrentSplit subsets log
    where
      concurrentSplitPerSubset :: [String] -> [(Int, [String])] -> [(Int, [String])]
      concurrentSplitPerSubset _ [] = []
      concurrentSplitPerSubset subset ((occ, trace):traces) = 
        (occ, replaceEmptyListWithTau (concurrentSplitPerTrace trace subset)) : 
        (concurrentSplitPerSubset subset traces)
          where
            concurrentSplitPerTrace :: [String] -> [String] -> [String]
            concurrentSplitPerTrace [] _ = []
            concurrentSplitPerTrace (x:xs) subset
              | elem x subset = x : (concurrentSplitPerTrace xs subset)
              | otherwise = (concurrentSplitPerTrace xs subset)
