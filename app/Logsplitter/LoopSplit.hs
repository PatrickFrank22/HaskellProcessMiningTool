{-
This implementation is based on:
Sander J. J. Leemans. Robust Process Mining with Guarantees - Process Discovery, Conformance Checking and Enhancement, volume 440 of Lecture Notes in Business Information Processing. Springer, 2022. ISBN 978-3-030-96654-6. doi: 10.1007/ 978-3-030-96655-3. https://doi.org/10.1007/978-3-030-96655-3.
-}

module Logsplitter.LoopSplit (
    loopSplit,
    loopSplitFiltering
) where

import Helperfunctions.HelperFunctions
import Data.List.Split (splitOn)
import Fallthrough.FallThrough (emptyTraces)


loopSplit :: [[String]] -> [(Int, [String])] -> [[(Int, [String])]]
loopSplit [] _ = []
loopSplit (subset:ss) log = [sumValuesByTraces (concatMap (\trace -> loopSplitPerTrace subset trace) log)] ++ loopSplit ss log
          
loopSplitPerTrace :: [String] -> (Int, [String]) -> [(Int, [String])]
loopSplitPerTrace subset (num, trace) = 
  addNumOfOccurrencesToTraces num (removeEmptyList (splitIntoSeparateTraces (markSplits trace subset)))
    where
      addNumOfOccurrencesToTraces :: Int -> [[String]] -> [(Int, [String])]
      addNumOfOccurrencesToTraces _ [] = []
      addNumOfOccurrencesToTraces num (x:xs) = (num, x) : (addNumOfOccurrencesToTraces num xs)

      splitIntoSeparateTraces :: [String] -> [[String]]
      splitIntoSeparateTraces trace = splitOn ["#"] trace

      markSplits [] _ = []
      markSplits (x:xs) subset
        | elem x subset = x : markSplits xs subset
        | otherwise = "#" : markSplits xs subset



loopSplitFiltering :: [[String]] -> [(Int, [String])] -> [[(Int, [String])]]
loopSplitFiltering (subset:ss) log = 
  let logCorrectStartEnd = sumValuesByTraces (concatMap (\(occ, x) -> addOccurrenceToAllListElements (occ, correctStartEndTrace x subset)) log)
      emptyTracesInLog = emptyTraces logCorrectStartEnd
  in if emptyTracesInLog /= [] then loopSplit ((subset ++ [tau]):ss) logCorrectStartEnd else loopSplit (subset:ss) logCorrectStartEnd
    where
      correctStartEndTrace :: [String] -> [String] -> [[String]]
      correctStartEndTrace trace body
        | traceCorrectStart && traceCorrectEnd = [trace]
        | traceCorrectStart = [trace] ++ [[tau]]
        | traceCorrectEnd = [[tau]] ++ [trace]
        | otherwise = [[tau]] ++ [trace] ++ [[tau]]
        -- | traceCorrectStart && traceCorrectEnd = [[tau]] ++ [trace] ++ [[tau]]
        -- | traceCorrectStart = [[tau]] ++ [trace]
        -- | traceCorrectEnd = [trace] ++ [[tau]]
        -- | otherwise = [trace]
            where 
              traceCorrectStart = elem (errorSafeHead trace) body
              traceCorrectEnd = elem (last trace) body
