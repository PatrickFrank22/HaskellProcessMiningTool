{-
This implementation is based on:
Sander J. J. Leemans. Robust Process Mining with Guarantees - Process Discovery, Conformance Checking and Enhancement, volume 440 of Lecture Notes in Business Information Processing. Springer, 2022. ISBN 978-3-030-96654-6. doi: 10.1007/ 978-3-030-96655-3. https://doi.org/10.1007/978-3-030-96655-3.
-}

module Fallthrough.FallThrough (
    emptyTraces,
    emptyTracesAboveThreshold,
    activityOncePerTrace,
    activityConcurrent,
    activityConcurrentIMF,
    strictTauLoop,
    tauLoop
) where

import Data.List
import Data.List.Split
import Helperfunctions.HelperFunctions
import Basecase.BaseCase
import Cutfinder.ExclusiveChoiceCut
import Cutfinder.SequenceCut
import Cutfinder.ConcurrentCut
import Cutfinder.LoopCut
import Logsplitter.ConcurrentSplit
import Cutfinder.LogFiltering


emptyTraces :: [(Int, [String])] -> [(Int, [String])]
emptyTraces log = 
  let logFreeOfTau = removeTauListTraceInLog log
  in if length logFreeOfTau < length log then logFreeOfTau else []



emptyTracesAboveThreshold :: Float -> [(Int, [String])] -> Bool
emptyTracesAboveThreshold _ [] = False
emptyTracesAboveThreshold f log = intToFloat (getNumOfOccurrencesEmptyTraces log) >= (intToFloat (lengthOfLog log)) * f
  where
    getNumOfOccurrencesEmptyTraces :: [(Int, [String])] -> Int
    getNumOfOccurrencesEmptyTraces [] = 0
    getNumOfOccurrencesEmptyTraces ((occ, trace):ts) 
      | trace == [tau] = occ
      | otherwise = getNumOfOccurrencesEmptyTraces ts

lengthOfLog :: [(Int, [String])] -> Int
lengthOfLog [] = 0
lengthOfLog ((occ, x):xs) = occ + lengthOfLog xs



activityOncePerTrace :: [(Int, [String])] -> [[String]]
activityOncePerTrace log = 
  let allAct = allActivities log
  in activityOncePerTraceHelper allAct allAct log
    where
      activityOncePerTraceHelper :: [String] -> [String] -> [(Int, [String])] -> [[String]]
      activityOncePerTraceHelper [] _ _ = []
      activityOncePerTraceHelper (x:xs) allAct log = 
        if activityOncePerTrace (errorSafeHead (concurrentSplit [[x]] log)) 
        then [[x], delete x allAct]
        else activityOncePerTraceHelper xs allAct log
          where
            activityOncePerTrace :: [(Int, [String])] -> Bool
            activityOncePerTrace [] = True
            activityOncePerTrace ((_,trace):ts) 
              | trace /= [tau] && length trace == 1 = activityOncePerTrace ts
              | otherwise = False



activityConcurrent :: [(Int, [String])] -> [[String]]
activityConcurrent [] = []
activityConcurrent log = 
  let allAct = allActivities log
  in activityConcurrentHelper allAct allAct log
    where
      activityConcurrentHelper :: [String] -> [String] -> [(Int, [String])] -> [[String]]
      activityConcurrentHelper [] _ _ = []
      activityConcurrentHelper (x:xs) allAct log  
        | testActivityConcurrentPerAct (errorSafeHead (concurrentSplit [delete x allAct] log))
            = [[x], delete x allAct]
        | otherwise = activityConcurrentHelper xs allAct log

testActivityConcurrentPerAct :: [(Int, [String])] -> Bool
testActivityConcurrentPerAct [] = False 
testActivityConcurrentPerAct log 
  | emptyTraces log /= [] = True
  | singleActivity log /= [] = True
  | atLeastTwoActivities && concat (xorCut allAct allTransitions) /= [] = True
  | atLeastTwoActivities && concat (sequenceCut allAct allTransitions) /= [] = True
  | atLeastTwoActivities && concat (concurrentCut allAct allTransitions startAct endAct) /= [] = True
  | atLeastTwoActivities && concat (loopCut allAct allTransitions startAct endAct) /= [] = True 
  | atLeastTwoActivities && concat (activityOncePerTrace log) /= [] = True
  | atLeastTwoActivities && strictTauLoop log /= [] = True
  | atLeastTwoActivities && tauLoop log /= [] = True
  | otherwise = False
      where
        allAct = allActivities log
        allTransitions = transitions log
        startAct = startActivities log
        endAct = endActivities log
        atLeastTwoActivities = length allAct >= 2



activityConcurrentIMF :: Float -> [(Int, [String])] -> [[String]]
activityConcurrentIMF _ [] = []
activityConcurrentIMF f log = 
  let allAct = allActivities log
  in activityConcurrentIMFHelper f allAct allAct log
    where
      activityConcurrentIMFHelper :: Float -> [String] -> [String] -> [(Int, [String])] -> [[String]]
      activityConcurrentIMFHelper _ [] _ _ = []
      activityConcurrentIMFHelper f (x:xs) allAct log  
        | testActivityConcurrentPerActIMF f (errorSafeHead (concurrentSplit [delete x allAct] log))
            = [[x], delete x allAct]
        | otherwise = activityConcurrentIMFHelper f xs allAct log

testActivityConcurrentPerActIMF :: Float -> [(Int, [String])] -> Bool
testActivityConcurrentPerActIMF _ [] = False 
testActivityConcurrentPerActIMF f log 
  -- | emptyTracesAboveThreshold f log && f > 0 = True -- not needed
  | emptyTraces log /= [] = True
  | singleActivity log /= [] = True
  | singleActivityFiltering f log /= [] = True
  | atLeastTwoActivities && concat (xorCut allAct allTransitions) /= [] = True
  | atLeastTwoActivities && concat (sequenceCut allAct allTransitions) /= [] = True
  | atLeastTwoActivities && concat (concurrentCut allAct allTransitions startAct endAct) /= [] = True
  | atLeastTwoActivities && concat (loopCut allAct allTransitions startAct endAct) /= [] = True 
  | atLeastTwoActivities && concat (xorCut allActFiltering allTransitionsFiltering) /= [] = True
  | atLeastTwoActivities && concat (sequenceCut allActFiltering allTransitionsFiltering) /= [] = True
  | atLeastTwoActivities && concat (concurrentCut allActFiltering allTransitionsFiltering startActFiltering endActFiltering) /= [] = True
  | atLeastTwoActivities && concat (loopCut allActFiltering allTransitionsFiltering startActFiltering endActFiltering) /= [] = True 
  | atLeastTwoActivities && concat (activityOncePerTrace log) /= [] = True
  | atLeastTwoActivities && strictTauLoop log /= [] = True
  | atLeastTwoActivities && tauLoop log /= [] = True
  | otherwise = False
      where
        allAct = allActivities log
        allTransitions = transitions log
        startAct = startActivities log
        endAct = endActivities log
        atLeastTwoActivities = length allAct >= 2
        (allActFiltering, allTransitionsFiltering, startActFiltering, endActFiltering) = logFiltering f log
        atLeastTwoActivitiesFilter = length allActFiltering >= 2



strictTauLoop :: [(Int, [String])] -> [(Int, [String])]
strictTauLoop log = 
  let startAct = startActivities log
      endAct = endActivities log
      resultingLog = sumValuesByTraces (strictTauLoopHelper log startAct endAct)
  in if lengthOfLog resultingLog > lengthOfLog log then resultingLog else []
    where
      strictTauLoopHelper :: [(Int, [String])] -> [String] -> [String] -> [(Int, [String])]
      strictTauLoopHelper [] _ _ = []
      strictTauLoopHelper ((occ, trace):traces) startAct endAct = 
        (splitListToTraces occ (splitTraceHelper (markSplits trace startAct endAct))) ++ strictTauLoopHelper traces startAct endAct
          where
            markSplits :: [String] -> [String] -> [String] -> [String]
            markSplits [] _ _ = []
            markSplits [x] _ _ = [x]
            markSplits (x1:x2:xs) startAct endAct
              | elem x1 endAct && elem x2 startAct = x1 : "#" : x2 : markSplits xs startAct endAct
              | otherwise = x1 : markSplits (x2:xs) startAct endAct

splitListToTraces :: Int -> [[String]] -> [(Int, [String])]
splitListToTraces _ [] = []
splitListToTraces num (x:xs) = (num, x) : (splitListToTraces num xs)

splitTraceHelper :: [String] -> [[String]]
splitTraceHelper trace = splitOn ["#"] trace



tauLoop :: [(Int, [String])] -> [(Int, [String])]
tauLoop log = 
  let startAct = startActivities log
      resultingLog = sumValuesByTraces (tauLoopHelper log startAct)
  in if lengthOfLog resultingLog > lengthOfLog log then resultingLog else []
    where
      tauLoopHelper :: [(Int, [String])] -> [String] -> [(Int, [String])]
      tauLoopHelper [] _ = []
      tauLoopHelper ((occ, trace):traces) startAct = 
        (splitListToTraces occ (splitTraceHelper (markSplits trace startAct))) ++ tauLoopHelper traces startAct
          where
            markSplits :: [String] -> [String] -> [String]
            markSplits [] _ = []
            markSplits [x] _ = [x]
            markSplits (x1:x2:xs) startAct
              | elem x2 startAct = x1 : "#" : markSplits (x2:xs) startAct
              | otherwise = x1 : markSplits (x2:xs) startAct
