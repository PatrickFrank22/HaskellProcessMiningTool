{-
This implementation is based on:
Sander J. J. Leemans. Robust Process Mining with Guarantees - Process Discovery, Conformance Checking and Enhancement, volume 440 of Lecture Notes in Business Information Processing. Springer, 2022. ISBN 978-3-030-96654-6. doi: 10.1007/ 978-3-030-96655-3. https://doi.org/10.1007/978-3-030-96655-3.
-}

module Logsplitter.ExclusiveChoiceSplit (
    xorSplit,
    xorSplitFiltering
) where

import Helperfunctions.HelperFunctions


xorSplit :: [[String]] -> [(Int, [String])] -> [[(Int, [String])]]
xorSplit [] _ = []
xorSplit (c:cs) eventLog = (sumValuesByTraces 
  (exclusiveChoiceSplitPerSubset c eventLog)) : (xorSplit cs eventLog)
    where
      exclusiveChoiceSplitPerSubset :: [String] -> [(Int, [String])] -> [(Int, [String])]
      exclusiveChoiceSplitPerSubset _ [] = []
      exclusiveChoiceSplitPerSubset c ((occ, (a:as)):ls) = 
        if elem a c 
        then (occ, (a:as)) : (exclusiveChoiceSplitPerSubset c ls) 
        else (exclusiveChoiceSplitPerSubset c ls)



xorSplitFiltering :: [[String]] -> [(Int, [String])] -> 
  [[(Int, [String])]]
xorSplitFiltering [] _ = []
xorSplitFiltering subsets log = 
  map (\x -> sumValuesByTraces (xorSplitFilteringPerSubset x log subsets)) subsets

xorSplitFilteringPerSubset :: [String] -> [(Int, [String])] -> [[String]] -> 
  [(Int, [String])]
xorSplitFilteringPerSubset _ [] _ = []
xorSplitFilteringPerSubset subset ((occ, trace):ts) subsets
  | xorTraceAssignment subsets (occ, trace) [] 0 == subset = 
      (occ, replaceEmptyListWithTau (removeActNotBelongingToSubset trace subset)) : 
      xorSplitFilteringPerSubset subset ts subsets
  | otherwise = xorSplitFilteringPerSubset subset ts subsets

xorTraceAssignment :: [[String]] -> (Int, [String]) -> [String] -> Int -> [String]
xorTraceAssignment [] _ dominantSubset _ = dominantSubset
xorTraceAssignment (subset:ss) (occ,trace) dominantSubset maxContainedAct
  | containedActPerSubset subset trace 0 > maxContainedAct = 
      xorTraceAssignment ss (occ,trace) subset (containedActPerSubset subset trace 0)
  | otherwise = xorTraceAssignment ss (occ,trace) dominantSubset maxContainedAct
      where
        containedActPerSubset :: [String] -> [String] -> Int -> Int
        containedActPerSubset [] _ count = count
        containedActPerSubset (a:as) subset count 
          | elem a subset = containedActPerSubset as subset (count + 1)
          | otherwise = containedActPerSubset as subset count
