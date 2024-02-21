{-
This implementation is based on:
Sander J. J. Leemans. Robust Process Mining with Guarantees - Process Discovery, Conformance Checking and Enhancement, volume 440 of Lecture Notes in Business Information Processing. Springer, 2022. ISBN 978-3-030-96654-6. doi: 10.1007/ 978-3-030-96655-3. https://doi.org/10.1007/978-3-030-96655-3.
-}

module Helperfunctions.HelperFunctions (
    tau,
    TreeNode(..), 
    startActivities, 
    endActivities, 
    allActivities, 
    transitions, 
    containsSameElement,
    removeTauListTraceInLog, 
    replaceEmptyListWithTau,
    removeEmptyList,
    removeActNotBelongingToSubset,
    createLogForEachAct,
    addOccurrenceToAllListElements,
    sumValuesByTraces,
    subsetLeadsToSubset, 
    intToFloat,
    preProcessingIM, 
    errorSafeHead,
    errorSafeTail,
    error
) where

import Data.Time
import Data.List
import Data.Ord (comparing)
import Control.Exception (throw)


tau :: String
tau = "Tau"


data TreeNode a = ExclusiveChoice [TreeNode a] | 
                  Sequence [TreeNode a] | 
                  Concurrent [TreeNode a] | 
                  Loop [TreeNode a] |
                  Activity a |
                  Activities a deriving Show


startActivities :: [(Int, [String])] -> [String]
startActivities = delete tau . sort . nub . map (\(_, list) -> errorSafeHead list)


endActivities :: [(Int, [String])] -> [String]
endActivities = delete tau . sort . nub . map (\(_, list) -> last list)


allActivities :: [(Int, [String])] -> [String]
allActivities log = delete tau (sort (nub (concat (concatAllTraces log))))


transitions :: [(Int, [String])] -> [(String, String)]
transitions log = sort (nub (concatMap transitionsHelper (concatAllTraces log)))
  where
    transitionsHelper :: [String] -> [(String, String)]
    transitionsHelper [] = []
    transitionsHelper [_] = []
    transitionsHelper (x:y:xs) 
      | x /= y = (x, y) : transitionsHelper (y:xs)
      | otherwise = transitionsHelper (y:xs)


containsSameElement :: [String] -> [String] -> Bool
containsSameElement xs ys = any (`elem` ys) xs


removeTauListTraceInLog :: [(Int, [String])] -> [(Int, [String])]
removeTauListTraceInLog [] = []
removeTauListTraceInLog (x:xs) 
  | snd x == [tau] = removeTauListTraceInLog xs
  | otherwise = x : removeTauListTraceInLog xs


replaceEmptyListWithTau :: [String] -> [String]
replaceEmptyListWithTau [] = [tau]
replaceEmptyListWithTau x = x


removeEmptyList :: [[String]] -> [[String]]
removeEmptyList [] = []
removeEmptyList (x:xs) = if x == [] then (removeEmptyList xs) else x : (removeEmptyList xs)


removeActNotBelongingToSubset :: [String] -> [String] -> [String]
removeActNotBelongingToSubset [] _ = []
removeActNotBelongingToSubset (a:as) subset
  | elem a subset = a : removeActNotBelongingToSubset as subset
  | otherwise = removeActNotBelongingToSubset as subset


{- only for the flowerModel -}
createLogForEachAct :: [String] -> [[(Int, [String])]]
createLogForEachAct [] = [[(1,[tau])]]
createLogForEachAct [x] = [[(1,[x])]]
createLogForEachAct (x:xs) = [(1,[x])] : (createLogForEachAct xs)


addOccurrenceToAllListElements :: (Int, [[String]]) -> [(Int, [String])]
addOccurrenceToAllListElements (count, lists) = map (\list -> (count, list)) lists

{-
sumValuesByTraces :: [(Int, [String])] -> [(Int, [String])]
sumValuesByTraces [] = []
sumValuesByTraces (trace:traces) = sumValuesByTracesHelper trace traces [] []
  where
    sumValuesByTracesHelper :: (Int, [String]) -> [(Int, [String])] -> [(Int, [String])] -> [(Int, [String])] -> [(Int, [String])]
    sumValuesByTracesHelper (xOcc, xTrace) [] [] result = ((xOcc, xTrace):result)
    sumValuesByTracesHelper (xOcc, xTrace) [] (r:remaining) result = sumValuesByTracesHelper r remaining [] ((xOcc, xTrace):result)
    sumValuesByTracesHelper (xOcc, xTrace) ((yOcc, yTrace):ys) remaining result
      | xTrace == yTrace = sumValuesByTracesHelper (xOcc + yOcc, xTrace) ys remaining result
      | otherwise = sumValuesByTracesHelper (xOcc, xTrace) ys ((yOcc, yTrace):remaining) result
-}

sumValuesByTraces :: [(Int, [String])] -> [(Int, [String])]
sumValuesByTraces [] = []
sumValuesByTraces log =
 let logSorted = sortBy (\a b -> compare (snd a) (snd b)) log --(\(_,xs) (_,ys) -> compare (length xs) (length ys)) log
 in sumValuesByTracesHelper (errorSafeHead logSorted) (drop 1 logSorted) [] []
  where
    sumValuesByTracesHelper :: (Int, [String]) -> [(Int, [String])] -> [(Int, [String])] -> [(Int, [String])] -> [(Int, [String])]
    sumValuesByTracesHelper (xOcc, xTrace) [] [] result = ((xOcc, xTrace):result)
    sumValuesByTracesHelper (xOcc, xTrace) [] (r:remaining) result = sumValuesByTracesHelper r remaining [] ((xOcc, xTrace):result)
    sumValuesByTracesHelper (xOcc, xTrace) ((yOcc, yTrace):ys) remaining result
      | xTrace == yTrace = sumValuesByTracesHelper (xOcc + yOcc, xTrace) ys remaining result
      | remaining /= [] = sumValuesByTracesHelper (errorSafeHead remaining) ((drop 1 remaining) ++ ((yOcc, yTrace):ys)) [] ((xOcc, xTrace):result)
      -- | length yTrace <= length xTrace && remaining /= [] = sumValuesByTracesHelper (head remaining) ((drop 1 remaining) ++ ((yOcc, yTrace):ys)) [] ((xOcc, xTrace):result)
      -- | length yTrace <= length xTrace = sumValuesByTracesHelper (yOcc, yTrace) ys [] ((xOcc, xTrace):result)
      | otherwise = sumValuesByTracesHelper (xOcc, xTrace) ys (remaining ++ [(yOcc, yTrace)]) result


subsetLeadsToSubset :: [String] -> [String] -> [(String, String)] -> Bool
subsetLeadsToSubset _ _ [] = False
subsetLeadsToSubset a b ((act1, act2):rest) = 
  if (elem act1 a && elem act2 b) 
  then True 
  else subsetLeadsToSubset a b rest


intToFloat :: Int -> Float
intToFloat x = fromIntegral x


preProcessingIM :: [[(String, ZonedTime)]] -> [(Int, [String])]
preProcessingIM log = sumValuesByTraces (addOccurrenceToAllListElements log)
  where
    addOccurrenceToAllListElements :: [[(String, ZonedTime)]] -> [(Int, [String])]
    addOccurrenceToAllListElements listsOfTuples = map (\list -> (1, map fst list)) listsOfTuples


errorSafeHead :: [a] -> a
errorSafeHead [] = error "Error: Function 'head' called with empty list."
errorSafeHead (x:_) = x


errorSafeTail :: [a] -> [a]
errorSafeTail [] = error "Error: Function 'tail' called with empty list."
errorSafeTail [x] = []
errorSafeTail (x:xs) = x : errorSafeTail xs


{- The following helper function is only used inside this module for the allActivities and transitions functions. -}
concatAllTraces :: [(Int, [String])] -> [[String]]
concatAllTraces [] = []
concatAllTraces tuples = map snd tuples
