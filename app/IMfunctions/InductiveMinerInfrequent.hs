{-
This implementation is based on:
Sander J. J. Leemans. Robust Process Mining with Guarantees - Process Discovery, Conformance Checking and Enhancement, volume 440 of Lecture Notes in Business Information Processing. Springer, 2022. ISBN 978-3-030-96654-6. doi: 10.1007/ 978-3-030-96655-3. https://doi.org/10.1007/978-3-030-96655-3.
-}

module IMfunctions.InductiveMinerInfrequent (
    inductiveMinerInfrequent
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
import Cutfinder.LogFiltering
import IMfunctions.InductiveMiner


inductiveMinerInfrequent :: Float -> [(Int, [String])] -> TreeNode String
inductiveMinerInfrequent f log = 
  if f <= 0.0 || f >= 1.0 then inductiveMiner log else processTreeReduction (inductiveMinerInfrequentMain f log)

inductiveMinerInfrequentMain :: Float -> [(Int, [String])] -> TreeNode String
inductiveMinerInfrequentMain _ [] = Activity tau
inductiveMinerInfrequentMain f log 
  | emptyTracesAboveThreshold_ = ExclusiveChoice [Activity tau, inductiveMinerInfrequent f (removeTauListTraceInLog log)]
  | empty /= [] = inductiveMinerInfrequent f (sumValuesByTraces (removeTauListTraceInLog log))
  | single /= [] = Activity (concat single)
  | singleFiltering /= [] = Activity (concat singleFiltering)
  | atLeastTwoActivities && concat exclusive /= [] = ExclusiveChoice (callIMF f (xorSplit exclusive log))
  | atLeastTwoActivities && concat sequence /= [] = Sequence (callIMF f (sequenceSplit sequence log))
  | atLeastTwoActivities && concat concurrent /= [] = Concurrent (callIMF f (concurrentSplit concurrent log))
  | atLeastTwoActivities && concat loop /= [] = Loop (callIMF f (loopSplit loop log))
  | atLeastTwoActivitiesFiltering && concat exclusiveFiltering /= [] = ExclusiveChoice (callIMF f (xorSplitFiltering exclusiveFiltering log)) --filteredLog))
  | atLeastTwoActivitiesFiltering && concat sequenceFiltering /= [] = Sequence (callIMF f (sequenceSplitFiltering sequenceFiltering log)) --filteredLog))
  | atLeastTwoActivitiesFiltering && concat concurrentFiltering /= [] = Concurrent (callIMF f (concurrentSplit concurrentFiltering log)) --filteredLog))
  | atLeastTwoActivitiesFiltering && concat loopFiltering /= [] = Loop (callIMF f (loopSplitFiltering loopFiltering log)) --filteredLog))
  | atLeastTwoActivities && concat activityOncePerTrace_ /= [] = Concurrent (callIMF f (concurrentSplit activityOncePerTrace_ log))
  | atLeastTwoActivities && concat activityConcurrentIMF_ /= [] = Concurrent (callIMF f (concurrentSplit activityConcurrentIMF_ log))
  | atLeastTwoActivities && strictTauLoop_ /= [] = Loop [inductiveMinerInfrequent f strictTauLoop_, Activity tau]
  | atLeastTwoActivities && tauLoop_ /= [] = Loop [inductiveMinerInfrequent f tauLoop_, Activity tau]
  | otherwise = Loop [ExclusiveChoice (callIMF f (createLogForEachAct allAct)), Activity tau] --flowerModel
      where 
        allAct = allActivities log
        allTransitions = transitions log
        startAct = startActivities log
        endAct = endActivities log
        single = singleActivity log
        singleFiltering = singleActivityFiltering f log
        emptyTracesAboveThreshold_ = emptyTracesAboveThreshold f log
        empty = emptyTraces log
        atLeastTwoActivities = length allAct >= 2
        exclusive = xorCut allAct allTransitions
        sequence = sequenceCut allAct allTransitions
        concurrent = concurrentCut allAct allTransitions startAct endAct
        loop = loopCut allAct allTransitions startAct endAct
        (allActFiltering, allTransitionsFiltering, startActFiltering, endActFiltering) = logFiltering f log
        atLeastTwoActivitiesFiltering = length allActFiltering >= 2
        exclusiveFiltering = xorCut allActFiltering allTransitionsFiltering
        sequenceFiltering = sequenceCut allActFiltering allTransitionsFiltering
        concurrentFiltering = concurrentCut allActFiltering allTransitionsFiltering startActFiltering endActFiltering
        loopFiltering = loopCut allActFiltering allTransitionsFiltering startActFiltering endActFiltering
        activityOncePerTrace_ = activityOncePerTrace log
        activityConcurrentIMF_ = activityConcurrentIMF f log
        strictTauLoop_ = strictTauLoop log
        tauLoop_ = tauLoop log

callIMF :: Float -> [[(Int, [String])]] -> [TreeNode String]
callIMF _ [] = []
callIMF f logs = map (\log -> inductiveMinerInfrequentMain f log) logs
