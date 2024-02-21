module Cutfinder.SequenceCut (
    sequenceCut,
    activityReach
) where

{-
This implementation is based on:
Sander J. J. Leemans. Robust Process Mining with Guarantees - Process Discovery, Conformance Checking and Enhancement, volume 440 of Lecture Notes in Business Information Processing. Springer, 2022. ISBN 978-3-030-96654-6. doi: 10.1007/ 978-3-030-96655-3. https://doi.org/10.1007/978-3-030-96655-3.
-}

import Data.List (delete, nub, sort)
import Helperfunctions.HelperFunctions (containsSameElement)
import Cutfinder.ExclusiveChoiceCut


sequenceCut :: [String] -> [(String, String)] -> [[String]]
sequenceCut allAct allTransitions = 
  let getReach = activityReach allAct allTransitions
      reachOrDoNotReach = concatMap (\x -> pairsReachOrDoNotReachEO x getReach) getReach
      subsets = xorCut allAct reachOrDoNotReach
      finalSubsets = mergeSubsetsIfReachEO subsets getReach
      sortedSubsets = sortSubsets finalSubsets getReach [] []
  in if length sortedSubsets == 1 then [] else sortedSubsets
    where
      pairsReachOrDoNotReachEO :: (String, [String]) -> [(String, [String])] -> [(String, String)]
      pairsReachOrDoNotReachEO _ [] = []
      pairsReachOrDoNotReachEO (x, xReach) ((y, yReach):ys) 
        | x /= y && elem x yReach && elem y xReach = 
            (x, y) : pairsReachOrDoNotReachEO (x, xReach) ys 
        | x /= y && not (elem x yReach) && not (elem y xReach) = 
            (x, y) : pairsReachOrDoNotReachEO (x, xReach) ys 
        | x /= y = pairsReachOrDoNotReachEO (x, xReach) ys
        | otherwise = pairsReachOrDoNotReachEO (x, xReach) ys  
      
      sortSubsets :: [[String]] -> [(String, [String])] -> [[String]] -> [[String]] -> [[String]]
      sortSubsets [] _ _ sorted = sorted
      sortSubsets [x] reachPerAct tested sorted = sortSubsets tested reachPerAct [] (x:sorted)
      sortSubsets (x1:x2:xs) reachPerAct tested sorted 
        | subsetReachesSubset x1 x2 reachPerAct = sortSubsets (x2:xs) reachPerAct (tested ++ [x1]) sorted
        | otherwise = sortSubsets (x1:xs) reachPerAct (tested ++ [x2]) sorted
            where
              subsetReachesSubset :: [String] -> [String] -> [(String, [String])] -> Bool
              subsetReachesSubset _ _ [] = False
              subsetReachesSubset x y ((z,zReach):zs) 
                | elem z x && containsSameElement y zReach = True
                | elem z y && containsSameElement x zReach = False
                | otherwise = subsetReachesSubset x y zs

activityReach :: [String] -> [(String, String)] -> [(String, [String])]
activityReach [] _ = []
activityReach (x:xs) pairs = 
  (x, nub (sort (activityReachForOneAct [x] pairs pairs))) : 
  (activityReach xs pairs)
    where
      activityReachForOneAct :: [String] -> [(String, String)] -> [(String, String)] -> [String]
      activityReachForOneAct x [] _ = x
      activityReachForOneAct x ((act1, act2):ys) pairs 
        | elem act1 x && not (elem act2 x) = activityReachForOneAct (act2:x) pairs pairs
        | otherwise = activityReachForOneAct x ys pairs


mergeSubsetsIfReachEO :: [[String]] -> [(String, [String])] -> [[String]]
mergeSubsetsIfReachEO subsets reachPerActList = 
  nub (map (\currentSubset -> mergeSubsetsIfReachEOHelper currentSubset 
  (delete currentSubset subsets) reachPerActList (delete currentSubset subsets)) subsets)
    where
      mergeSubsetsIfReachEOHelper :: [String] -> [[String]] -> [(String, [String])] -> [[String]] -> [String]
      mergeSubsetsIfReachEOHelper mainSubset [] _ _ = sort mainSubset
      mergeSubsetsIfReachEOHelper mainSubset (otherSubset:restSubsets) reachPerActList remainingSubsets
        | testIfTwoSubsetsReachEO mainSubset otherSubset reachPerActList = 
            mergeSubsetsIfReachEOHelper (mainSubset ++ otherSubset) 
            (delete otherSubset remainingSubsets) reachPerActList (delete otherSubset remainingSubsets)
        | otherwise = mergeSubsetsIfReachEOHelper mainSubset restSubsets reachPerActList remainingSubsets
            where
              testIfTwoSubsetsReachEO :: [String] -> [String] -> [(String, [String])] -> Bool
              testIfTwoSubsetsReachEO _ _ [] = False
              testIfTwoSubsetsReachEO mainSubset otherSubset ((x, xReach):restReach)
                | (elem x mainSubset && containsSameElement otherSubset xReach) &&
                  (elem x otherSubset && containsSameElement mainSubset xReach) = True
                | otherwise = testIfTwoSubsetsReachEO mainSubset otherSubset restReach
