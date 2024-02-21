{-
This implementation is based on:
Sander J. J. Leemans. Robust Process Mining with Guarantees - Process Discovery, Conformance Checking and Enhancement, volume 440 of Lecture Notes in Business Information Processing. Springer, 2022. ISBN 978-3-030-96654-6. doi: 10.1007/ 978-3-030-96655-3. https://doi.org/10.1007/978-3-030-96655-3.
-}

module IMfunctions.InductiveMiner (
    inductiveMiner
) where

import Helperfunctions.HelperFunctions
import Basecase.BaseCase
import Cutfinder.ExclusiveChoiceCut
import Cutfinder.SequenceCut
import Cutfinder.ConcurrentCut
import Cutfinder.LoopCut
import Fallthrough.FallThrough
import Logsplitter.ExclusiveChoiceSplit
import Logsplitter.SequenceSplit
import Logsplitter.ConcurrentSplit
import Logsplitter.LoopSplit
import Processtreereduction.ProcessTreeReduction


inductiveMiner :: [(Int, [String])] -> TreeNode String
inductiveMiner log = processTreeReduction (inductiveMinerMain log)

inductiveMinerMain :: [(Int, [String])] -> TreeNode String
inductiveMinerMain [] = Activity tau
inductiveMinerMain log 
  | emptyTraces_ /= [] = ExclusiveChoice [inductiveMinerMain (removeTauListTraceInLog log), Activity tau]
  | singleActivity_ /= [] = Activity (concat singleActivity_)
  | atLeastTwoActivities && concat xorCut_ /= [] = ExclusiveChoice (callIM (xorSplit xorCut_ log))
  | atLeastTwoActivities && concat sequenceCut_ /= [] = Sequence (callIM (sequenceSplit sequenceCut_ log))
  | atLeastTwoActivities && concat concurrentCut_ /= [] = Concurrent (callIM (concurrentSplit concurrentCut_ log))
  | atLeastTwoActivities && concat loopCut_ /= [] = Loop (callIM (loopSplit loopCut_ log))
  | atLeastTwoActivities && concat activityOncePerTrace_ /= [] = Concurrent (callIM (concurrentSplit activityOncePerTrace_ log))
  | atLeastTwoActivities && concat activityConcurrent_ /= [] = Concurrent (callIM (concurrentSplit activityConcurrent_ log))
  | atLeastTwoActivities && strictTauLoop_ /= [] = Loop [inductiveMinerMain (sumValuesByTraces strictTauLoop_), Activity tau]
  | atLeastTwoActivities && tauLoop_ /= [] = Loop [inductiveMinerMain (sumValuesByTraces tauLoop_), Activity tau]
  | otherwise = Loop [ExclusiveChoice (callIM (createLogForEachAct allAct)), Activity tau] --flowerModel
        where 
          allAct = allActivities log
          allTransitions = transitions log
          startAct = startActivities log
          endAct = endActivities log
          singleActivity_ = singleActivity log
          emptyTraces_ = emptyTraces log
          atLeastTwoActivities = length allAct >= 2
          xorCut_ = xorCut allAct allTransitions
          sequenceCut_ = sequenceCut allAct allTransitions
          concurrentCut_ = concurrentCut allAct allTransitions startAct endAct
          loopCut_ = loopCut allAct allTransitions startAct endAct
          activityOncePerTrace_ = activityOncePerTrace log
          activityConcurrent_ = activityConcurrent log
          strictTauLoop_ = strictTauLoop log
          tauLoop_ = tauLoop log

callIM :: [[(Int, [String])]] -> [TreeNode String]
callIM [] = []
callIM logs = map inductiveMinerMain logs
