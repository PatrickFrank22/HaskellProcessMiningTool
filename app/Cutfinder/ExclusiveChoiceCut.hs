{-
This implementation is based on:
Sander J. J. Leemans. Robust Process Mining with Guarantees - Process Discovery, Conformance Checking and Enhancement, volume 440 of Lecture Notes in Business Information Processing. Springer, 2022. ISBN 978-3-030-96654-6. doi: 10.1007/ 978-3-030-96655-3. https://doi.org/10.1007/978-3-030-96655-3.
-}

module Cutfinder.ExclusiveChoiceCut (
    xorCut
) where

import Data.List (delete, nub, sort)


xorCut :: [String] -> [(String, String)] -> [[String]]
xorCut [] _ = []
xorCut [x] _ = []
xorCut allAct allTransitions = 
  let subsets = getSubsets allAct allTransitions
  in if length subsets == 1 then [] else subsets
    where
      getSubsets :: [String] -> [(String, String)] -> [[String]]
      getSubsets [] _ = []
      getSubsets (x:remainingAct) transitions = 
        let (resultingSubset, remainingTransitions) = mergeConnectedAct [x] transitions transitions
        in resultingSubset : (getSubsets (removeSubsetFromSubset resultingSubset remainingAct) remainingTransitions)
          where
            mergeConnectedAct :: [String] -> [(String, String)] -> [(String, String)] -> ([String], [(String, String)])
            mergeConnectedAct x [] remainingTransitions = (sort x, remainingTransitions)
            mergeConnectedAct x ((act1, act2):ys) transitions 
              | elem act1 x && not (elem act2 x) = mergeConnectedAct (act2:x) (delete (act1, act2) transitions) (delete (act1, act2) transitions)
              | elem act2 x && not (elem act1 x) = mergeConnectedAct (act1:x) (delete (act1, act2) transitions) (delete (act1, act2) transitions)
              | otherwise = mergeConnectedAct x ys transitions

            removeSubsetFromSubset :: [String] -> [String] -> [String]
            removeSubsetFromSubset _ [] = []
            removeSubsetFromSubset xs (y:ys)
              | elem y xs = removeSubsetFromSubset xs ys
              | otherwise = y : removeSubsetFromSubset xs ys
