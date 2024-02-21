{-
This implementation is based on:
Sander J. J. Leemans. Robust Process Mining with Guarantees - Process Discovery, Conformance Checking and Enhancement, volume 440 of Lecture Notes in Business Information Processing. Springer, 2022. ISBN 978-3-030-96654-6. doi: 10.1007/ 978-3-030-96655-3. https://doi.org/10.1007/978-3-030-96655-3.
-}

module Processtreereduction.ProcessTreeReduction (
    processTreeReduction
) where

import Helperfunctions.HelperFunctions (TreeNode(..), errorSafeHead)


processTreeReduction :: TreeNode String -> TreeNode String
processTreeReduction processTree = 
  let newProcessTree = singularityRule (tauReductionRules (associativityReductionRules processTree))
  in if (not (equalProcessTrees processTree newProcessTree)) then (processTreeReduction newProcessTree) else processTree

equalProcessTrees :: TreeNode String -> TreeNode String -> Bool
equalProcessTrees (Activity x) (Activity y) = if x == y then True else False
equalProcessTrees (ExclusiveChoice x) (ExclusiveChoice y) = if length x == length y then (callEqualProcessTrees x y) else False
equalProcessTrees (Sequence x) (Sequence y) = if length x == length y then (callEqualProcessTrees x y) else False
equalProcessTrees (Concurrent x) (Concurrent y) = if length x == length y then (callEqualProcessTrees x y) else False
equalProcessTrees (Loop x) (Loop y) = if length x == length y then (callEqualProcessTrees x y) else False
equalProcessTrees _ _ = False

callEqualProcessTrees :: [TreeNode String] -> [TreeNode String] -> Bool
callEqualProcessTrees [] [] = True
callEqualProcessTrees (x:xs) (y:ys) = equalProcessTrees x y && callEqualProcessTrees xs ys



{- associativityReductionRules -}

associativityReductionRules :: TreeNode String -> TreeNode String
associativityReductionRules (Activity a) = Activity a
associativityReductionRules (ExclusiveChoice a) = 
  ExclusiveChoice (callAssociativityReductionRules (containsXOR a []))
associativityReductionRules (Sequence a) = 
  Sequence (callAssociativityReductionRules (containsSequence a []))
associativityReductionRules (Concurrent a) = 
  Concurrent (callAssociativityReductionRules (containsConcurrent a []))
associativityReductionRules (Loop a) = 
  Loop (callAssociativityReductionRules ([containsLoop1 (errorSafeHead a)] ++ 
    (containsXOR (removeFirstElementInList a) [])))

callAssociativityReductionRules :: [TreeNode String] -> [TreeNode String] 
callAssociativityReductionRules [] = []
callAssociativityReductionRules (a:as) = associativityReductionRules a : 
  (callAssociativityReductionRules as)

containsXOR :: [TreeNode String] -> [TreeNode String] -> [TreeNode String]
containsXOR [] res = res
containsXOR ((ExclusiveChoice a):as) res = containsXOR as (res ++ (containsXOR a []))
containsXOR (a:as) res = containsXOR as (res ++ [a])

containsSequence :: [TreeNode String] -> [TreeNode String] -> [TreeNode String]
containsSequence [] res = res
containsSequence ((Sequence a):as) res = containsSequence as (res ++ 
  (containsSequence a []))
containsSequence (a:as) res = containsSequence as (res ++ [a])

containsConcurrent :: [TreeNode String] -> [TreeNode String] -> [TreeNode String]
containsConcurrent [] res = res
containsConcurrent ((Concurrent a):as) res = containsConcurrent as (res ++ 
  (containsConcurrent a []))
containsConcurrent (a:as) res = containsConcurrent as (res ++ [a])

containsLoop1 :: TreeNode String -> TreeNode String
containsLoop1 (Loop a) = containsLoop1 (errorSafeHead a)
containsLoop1 a = a

removeFirstElementInList :: [TreeNode String] -> [TreeNode String]
removeFirstElementInList [] = []
removeFirstElementInList (y:ys) = ys



{- tauReductionRules -}

tauReductionRules :: TreeNode String -> TreeNode String
tauReductionRules (Activity a) = Activity a
tauReductionRules (ExclusiveChoice a)
  | isActivityTau (orRelationship (removeTauAct a)) = ExclusiveChoice (callTauReductionRules (removeTauAct a))
  | otherwise = ExclusiveChoice (callTauReductionRules a)
tauReductionRules (Sequence a)  
  | isNotEmptyList (removeTauAct a) = Sequence (callTauReductionRules (removeTauAct a))
  | otherwise = Sequence (callTauReductionRules a)
tauReductionRules (Concurrent a)
  | isNotEmptyList (removeTauAct a) = Concurrent (callTauReductionRules (removeTauAct a))
  | otherwise = Concurrent (callTauReductionRules a)
tauReductionRules (Loop [(Activity "Tau"),(Activity "Tau")]) = (Activity "Tau")
tauReductionRules (Loop a)
  | isActivityTau (errorSafeHead a) && not (isActivityTau (isTauDirectSuccessor a)) =
      ExclusiveChoice [Activity "Tau", Loop [tauReductionRules (ExclusiveChoice (removeFirstElementInList a)), Activity "Tau"]]
  | isActivityTau (orRelationship (removeTauAct (removeFirstElementInList a))) = 
      Loop (callTauReductionRules ([errorSafeHead a] ++ (removeTauAct (removeFirstElementInList a))))
  | otherwise = Loop (callTauReductionRules a)

callTauReductionRules :: [TreeNode String] -> [TreeNode String] 
callTauReductionRules [] = []
callTauReductionRules (a:as) = tauReductionRules a : (callTauReductionRules as)

removeTauAct :: [TreeNode String] -> [TreeNode String]
removeTauAct [] = []
removeTauAct ((Activity "Tau"):as) = removeTauAct as
removeTauAct (a:as) = a : removeTauAct as

tauReachable :: TreeNode String -> TreeNode String 
tauReachable (Activity a) = (Activity a)
tauReachable (ExclusiveChoice a) = orRelationship a 
tauReachable (Sequence a) = andRelationship a 
tauReachable (Concurrent a) = andRelationship a
tauReachable (Loop a) = errorSafeHead a

andRelationship :: [TreeNode String] -> TreeNode String 
andRelationship [] = (Activity "Tau")
andRelationship ((Activity "Tau"):as) = andRelationship as
andRelationship (a:as) | isActivityTau (tauReachable a) = andRelationship as
                       | otherwise = (Activity "notTau")

orRelationship :: [TreeNode String] -> TreeNode String
orRelationship [] = (Activity "notTau")
orRelationship ((Activity "Tau"):as) = (Activity "Tau")
orRelationship (a:as) | isActivityTau (tauReachable a) = (Activity "Tau")
                      | otherwise = orRelationship as

isTauDirectSuccessor :: [TreeNode String] -> TreeNode String
isTauDirectSuccessor [] = (Activity "notTau")
isTauDirectSuccessor ((Activity "Tau"):as) = (Activity "Tau")
isTauDirectSuccessor (a:as) = isTauDirectSuccessor as

isActivityTau :: TreeNode String -> Bool
isActivityTau (Activity "Tau") = True
isActivityTau _ = False

isNotEmptyList :: [TreeNode String] -> Bool
isNotEmptyList [] = False
isNotEmptyList _ = True



{- singularityRule -}

singularityRule :: TreeNode String -> TreeNode String
singularityRule (Activity a) = Activity a
singularityRule (ExclusiveChoice [a]) = singularityRule a
singularityRule (ExclusiveChoice a) = ExclusiveChoice (callSingularityRule a)
singularityRule (Sequence [a]) = singularityRule a
singularityRule (Sequence a) = Sequence (callSingularityRule a)
singularityRule (Concurrent [a]) = singularityRule a
singularityRule (Concurrent a) = Concurrent (callSingularityRule a)
singularityRule (Loop [a]) = singularityRule a
singularityRule (Loop a) = Loop (callSingularityRule a)

callSingularityRule :: [TreeNode String] -> [TreeNode String] 
callSingularityRule [] = []
callSingularityRule (a:as) = singularityRule a : (callSingularityRule as)
