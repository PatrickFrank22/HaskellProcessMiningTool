module Cutfinder.ConcurrentCut (
    concurrentCut
) where

{-
This implementation is based on:
Sander J. J. Leemans. Robust Process Mining with Guarantees - Process Discovery, Conformance Checking and Enhancement, volume 440 of Lecture Notes in Business Information Processing. Springer, 2022. ISBN 978-3-030-96654-6. doi: 10.1007/ 978-3-030-96655-3. https://doi.org/10.1007/978-3-030-96655-3.
-}

import Data.List (delete, nub, sort)
import Helperfunctions.HelperFunctions (containsSameElement)
import Cutfinder.ExclusiveChoiceCut


concurrentCut :: [String] -> [(String, String)] -> [String] -> [String] -> [[String]]
concurrentCut allAct allTransitions startAct endAct = 
  let possibleTransitions = allPossibleTransitions allAct
      oppositeTransitions = getOppositeTransitions possibleTransitions allTransitions
      subsets = xorCut allAct oppositeTransitions
      subsetsContainStartEnd = getSubsetsContainStartEnd subsets startAct endAct
  in if length subsetsContainStartEnd == 1 then [] else subsetsContainStartEnd
    where
      getOppositeTransitions :: [(String, String)] -> [(String, String)] -> [(String, String)]
      getOppositeTransitions [] _ = []
      getOppositeTransitions (x:xs) allTransitions 
        | elem x allTransitions = getOppositeTransitions xs allTransitions 
        | otherwise = x : getOppositeTransitions xs allTransitions 

      allPossibleTransitions :: [String] -> [(String, String)]
      allPossibleTransitions allAct = concatMap (\x -> allPossibleTransitionsHelper x (delete x allAct)) allAct
        where
          allPossibleTransitionsHelper :: String -> [String] -> [(String, String)]
          allPossibleTransitionsHelper x allActButX = map (\y -> (x,y)) allActButX

getSubsetsContainStartEnd :: [[String]] -> [String] -> [String] -> [[String]]
getSubsetsContainStartEnd subsets startAct endAct = 
  mergeSubsetsOnStartEnd (getStartEndParts subsets startAct endAct ([], [], [], []))
    where
      getStartEndParts :: [[String]] -> [String] -> [String] -> 
        ([[String]], [[String]], [[String]], [[String]]) -> 
        ([[String]], [[String]], [[String]], [[String]])
      getStartEndParts [] _ _ (startAndEnd, onlyStart, onlyEnd, noStartEnd) = 
        (startAndEnd, onlyStart, onlyEnd, noStartEnd)
      getStartEndParts (x:xs) startAct endAct 
        (startAndEnd, onlyStart, onlyEnd, noStartEnd) 
          | containsSameElement x startAct && containsSameElement x endAct = 
              getStartEndParts xs startAct endAct 
              ((x:startAndEnd), onlyStart, onlyEnd, noStartEnd)
          | containsSameElement x startAct = 
              getStartEndParts xs startAct endAct 
              (startAndEnd, (x:onlyStart), onlyEnd, noStartEnd)
          | containsSameElement x endAct = 
              getStartEndParts xs startAct endAct 
              (startAndEnd, onlyStart, (x:onlyEnd), noStartEnd)
          | otherwise = 
              getStartEndParts xs startAct endAct 
              (startAndEnd, onlyStart, onlyEnd, (x:noStartEnd))

      mergeSubsetsOnStartEnd :: ([[String]], [[String]], [[String]], [[String]]) -> [[String]]
      mergeSubsetsOnStartEnd (startAndEnd, [], [], []) = startAndEnd
      mergeSubsetsOnStartEnd ((se:startAndEnd), [], (e:onlyEnd), []) = 
        mergeSubsetsOnStartEnd (((se ++ e):startAndEnd), [], onlyEnd, []) 
      mergeSubsetsOnStartEnd ((se:startAndEnd), (s:onlyStart), [], []) = 
        mergeSubsetsOnStartEnd (((se ++ s):startAndEnd), onlyStart, [], []) 
      mergeSubsetsOnStartEnd (startAndEnd, (s:onlyStart), (e:onlyEnd), []) = 
        mergeSubsetsOnStartEnd (((s ++ e):startAndEnd), onlyStart, onlyEnd, []) 
      mergeSubsetsOnStartEnd ((se:startAndEnd), onlyStart, onlyEnd, (n:noStartEnd)) = 
        mergeSubsetsOnStartEnd (((se ++ n):startAndEnd), onlyStart, onlyEnd, noStartEnd)
