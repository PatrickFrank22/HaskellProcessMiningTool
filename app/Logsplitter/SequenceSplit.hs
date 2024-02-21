{-
This implementation is based on:
Sander J. J. Leemans. Robust Process Mining with Guarantees - Process Discovery, Conformance Checking and Enhancement, volume 440 of Lecture Notes in Business Information Processing. Springer, 2022. ISBN 978-3-030-96654-6. doi: 10.1007/ 978-3-030-96655-3. https://doi.org/10.1007/978-3-030-96655-3.
-}

module Logsplitter.SequenceSplit (
    sequenceSplit,
    sequenceSplitFiltering
) where

import Data.List
import Helperfunctions.HelperFunctions
import Logsplitter.ExclusiveChoiceSplit
import Logsplitter.ConcurrentSplit


sequenceSplit :: [[String]] -> [(Int, [String])] -> [[(Int, [String])]]
sequenceSplit = concurrentSplit 



sequenceSplitFiltering :: [[String]] -> [(Int, [String])] -> [[(Int, [String])]]
sequenceSplitFiltering subsets log = 
  let splitLog = findSplitsPerLog subsets log
  in map (\x -> sumValuesByTraces (getTracesBelongingToSubset x splitLog)) subsets
    where
      getTracesBelongingToSubset :: [String] -> [(Int, ([String], [String]))] -> [(Int, [String])]
      getTracesBelongingToSubset _ [] = []
      getTracesBelongingToSubset currentSubset ((occ,(subset, trace)):ts) 
        | subset == currentSubset = (occ, trace) : getTracesBelongingToSubset currentSubset ts
        | otherwise = getTracesBelongingToSubset currentSubset ts

findSplitsPerLog :: [[String]] -> [(Int, [String])] -> [(Int, ([String], [String]))]
findSplitsPerLog subsets log = concatMap (\x -> findSplitsPerTrace subsets x []) log
  where
    findSplitsPerTrace :: [[String]] -> (Int, [String]) -> [String] -> [(Int, ([String], [String]))]
    findSplitsPerTrace [] _ _ = []
    findSplitsPerTrace (currentSubset:ss) (occ, trace) ignoredSubsets = 
      let splitTraces = possibleSplitTraces ("placeholder":trace) trace 0
          (leftOptimal, rightOptimal) = getLeastDeviatingSplit splitTraces currentSubset ignoredSubsets ([],[]) ((length trace)+1)
          resultingTrace = removeActNotBelongingToSubset leftOptimal currentSubset
      in (occ,(currentSubset, replaceEmptyListWithTau resultingTrace)) : 
           findSplitsPerTrace ss (occ, rightOptimal) (nub (currentSubset ++ ignoredSubsets))
        where
          possibleSplitTraces :: [String] -> [String] -> Int -> [([String], [String])]
          possibleSplitTraces [] trace _ = []
          possibleSplitTraces (a:as) trace count =
            (take count trace, drop count trace) : possibleSplitTraces as trace (count + 1)

getLeastDeviatingSplit :: [([String], [String])] -> [String] -> [String] -> ([String], [String]) -> Int -> ([String], [String])
getLeastDeviatingSplit [] _ _ (leftOptimal, rightOptimal) _ = (leftOptimal, rightOptimal)
getLeastDeviatingSplit (splitTrace:rest) currentSubset ignoredSubsets optimalSplitSubset minDeviation
  | numberOfDeviations splitTrace currentSubset ignoredSubsets < minDeviation = 
      let newMinDeviation = numberOfDeviations splitTrace currentSubset ignoredSubsets
      in getLeastDeviatingSplit rest currentSubset ignoredSubsets splitTrace newMinDeviation
  | otherwise = getLeastDeviatingSplit rest currentSubset ignoredSubsets optimalSplitSubset minDeviation
      where
        numberOfDeviations :: ([String], [String]) -> [String] -> [String] -> Int
        numberOfDeviations (left, right) subset ignore =
          numberOfDeviationsLeft left subset ignore 0 + numberOfDeviationsRight right subset ignore 0
            where
              numberOfDeviationsLeft :: [String] -> [String] -> [String] -> Int -> Int
              numberOfDeviationsLeft [] _ _ cost = cost
              numberOfDeviationsLeft (a:as) subset ignore cost | elem a subset = 
                numberOfDeviationsLeft as subset ignore (cost - 1)
              numberOfDeviationsLeft (a:as) subset ignore cost | elem a ignore = 
                numberOfDeviationsLeft as subset ignore cost
              numberOfDeviationsLeft (a:as) subset ignore cost | otherwise = 
                numberOfDeviationsLeft as subset ignore (cost + 1)

              numberOfDeviationsRight :: [String] -> [String] -> [String] -> Int -> Int
              numberOfDeviationsRight [] _ _ cost = cost
              numberOfDeviationsRight (a:as) subset ignore cost | elem a subset = 
                numberOfDeviationsRight as subset ignore (cost + 1)
              numberOfDeviationsRight (a:as) subset ignore cost | elem a ignore = 
                numberOfDeviationsRight as subset ignore cost
              numberOfDeviationsRight (a:as) subset ignore cost | otherwise = 
                numberOfDeviationsRight as subset ignore (cost - 1)
