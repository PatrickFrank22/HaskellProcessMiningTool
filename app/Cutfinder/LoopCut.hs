module Cutfinder.LoopCut (
    loopCut
) where

{-
This implementation is based on:
Sander J. J. Leemans. Robust Process Mining with Guarantees - Process Discovery, Conformance Checking and Enhancement, volume 440 of Lecture Notes in Business Information Processing. Springer, 2022. ISBN 978-3-030-96654-6. doi: 10.1007/ 978-3-030-96655-3. https://doi.org/10.1007/978-3-030-96655-3.
-}

import Data.List (delete, nub, sort)
import Cutfinder.ExclusiveChoiceCut
import Helperfunctions.HelperFunctions


loopCut :: [String] -> [(String, String)] -> [String] -> [String] -> [[String]]
loopCut allAct allTransitions startAct endAct = 
  let bodyPart = nub (startAct ++ endAct)
      loopPartsActivities = getLoopPartsActivities allAct bodyPart
      loopPartsViaXorCut = xorCut loopPartsActivities allTransitions
      loopParts = if loopPartsViaXorCut /= [] then loopPartsViaXorCut else [loopPartsActivities]
      finalSubsets = testLoopCutConditions loopParts allTransitions startAct endAct bodyPart []
  in if length finalSubsets == 1 then [] else finalSubsets
    where
      getLoopPartsActivities :: [String] -> [String] -> [String]
      getLoopPartsActivities [] _ = []
      getLoopPartsActivities (a:as) bodyPart
        | elem a bodyPart = getLoopPartsActivities as bodyPart
        | otherwise = a : getLoopPartsActivities as bodyPart

      testLoopCutConditions :: [[String]] -> [(String, String)] -> [String] -> [String] -> [String] -> [[String]] -> [[String]]
      testLoopCutConditions [] _ _ _ bodyPart loopPart = ((sort bodyPart):loopPart)
      testLoopCutConditions (x:xs) allTransitions startAct endAct bodyPart loopPart
        | x /= [] && endToLoopPartStartToBodyPart x allTransitions startAct endAct x &&
            connectionToAllStartAct x startAct allTransitions && connectionFromAllEndAct x endAct allTransitions
              = testLoopCutConditions xs allTransitions startAct endAct bodyPart (x:loopPart)
        | otherwise = testLoopCutConditions xs allTransitions startAct endAct (x ++ bodyPart) loopPart

connectionToAllStartAct :: [String] -> [String] -> [(String, String)] -> Bool
connectionToAllStartAct [] _ _ = True
connectionToAllStartAct (a:as) startAct allTransitions 
  | actLeadsToAllStartAct a startAct allTransitions allTransitions || 
      not (subsetLeadsToSubset [a] startAct allTransitions) =
        connectionToAllStartAct as startAct allTransitions 
  | otherwise = False
      where 
        actLeadsToAllStartAct :: String -> [String] -> [(String, String)] -> 
          [(String, String)] -> Bool
        actLeadsToAllStartAct _ [] _ _ = True
        actLeadsToAllStartAct _ _ [] _ = False
        actLeadsToAllStartAct b (a:as) ((act1, act2):rest) allTransitions 
          | (act1 == b && act2 == a) = actLeadsToAllStartAct b as allTransitions allTransitions 
          | otherwise = actLeadsToAllStartAct b (a:as) rest allTransitions

connectionFromAllEndAct :: [String] -> [String] -> [(String, String)] -> Bool
connectionFromAllEndAct [] _ _ = True
connectionFromAllEndAct (a:as) endAct allTransitions 
  | allEndActLeadToAct endAct a allTransitions allTransitions || 
      not (subsetLeadsToSubset endAct [a] allTransitions) =
        connectionFromAllEndAct as endAct allTransitions 
  | otherwise = False
      where
        allEndActLeadToAct :: [String] -> String -> [(String, String)] -> [(String, String)] -> Bool
        allEndActLeadToAct [] _ _ _ = True
        allEndActLeadToAct _ _ [] _ = False
        allEndActLeadToAct (e:es) b ((act1, act2):rest) allTransitions 
          | act1 == e && act2 == b = allEndActLeadToAct es b allTransitions allTransitions 
          | otherwise = allEndActLeadToAct (e:es) b rest allTransitions

endToLoopPartStartToBodyPart :: [String] -> [(String, String)] -> [String] -> [String] -> [String] -> Bool
endToLoopPartStartToBodyPart [] _ _ _ _ = True
endToLoopPartStartToBodyPart (x:xs) allTransitions startAct endAct currentLoopPart
  | endToLoopPart x currentLoopPart endAct allTransitions && 
      startToBodyPart x currentLoopPart startAct allTransitions = 
        endToLoopPartStartToBodyPart xs allTransitions startAct endAct currentLoopPart
  | otherwise = False
      where
        endToLoopPart _ _ _ [] = True
        endToLoopPart x currentLoopPart endAct ((act1, act2):rest) = 
          if act2 == x && not (elem act1 currentLoopPart) 
          then (if (elem act1 endAct) 
                then (endToLoopPart x currentLoopPart endAct rest) 
                else False)
          else endToLoopPart x currentLoopPart endAct rest

        startToBodyPart _ _ _ [] = True
        startToBodyPart x currentLoopPart startAct ((act1, act2):rest) = 
          if act1 == x && not (elem act2 currentLoopPart) 
          then (if (elem act2 startAct) 
                then (startToBodyPart x currentLoopPart startAct rest) 
                else False)
          else startToBodyPart x currentLoopPart startAct rest
