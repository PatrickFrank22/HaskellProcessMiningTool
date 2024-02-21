module MainConsole.MainConsole ( mainConsole, callMiningAlgorithm ) where

import Parser.XesParserXmlConduit
import Parser.XesParserXmlConduitPrefix
import Parser.XesParserHxt
import Parser.CsvParserCassava
import IMfunctions.InductiveMiner
import IMfunctions.InductiveMinerInfrequent
import Helperfunctions.HelperFunctions
import ProcessTreeVisualization.DrawProcessTree (mainDrawProcessTree)
import Data.Time


mainConsole :: IO ()
mainConsole = do
    putStrLn "Please select either CSV (1), XES xml-conduit (2), XES xml-conduit prefix version (3) or XES hxt (4) as a parser (without \"\")."
    selectedParser <- getLine
    putStrLn "Please select either Inductive Miner (1) or Inductive Miner - Infrequent (2) as algorithm (without \"\")."
    selectedMiner <- getLine
    putStrLn "Please choose a Noise Threshold in between 0.0 and 1.0 (if the Inductive Miner - Infrequent is selected) (without \"\")."
    threshold <- getLine  
    
    parserResult <- callParser selectedParser
    let log = (preProcessingIM parserResult)
    if log == [] then putStrLn "Parsing error!" else putStrLn ""

    let minerResult = callMiningAlgorithm selectedMiner threshold parserResult
    mainDrawProcessTree minerResult


callParser :: String -> IO [[(String, ZonedTime)]]
callParser "CSV" = csvParser
callParser "XES xml-conduit" = xesParserXmlConduit
callParser "XES xml-conduit prefix version" = xesParserXmlConduitPrefix
callParser "XES hxt" = xesParserHxt
callParser "1" = csvParser
callParser "2" = xesParserXmlConduit
callParser "3" = xesParserXmlConduitPrefix
callParser "4" = xesParserHxt
callParser _ = error "Selected Parser has not been implemented."

callMiningAlgorithm :: String -> String -> [[(String, ZonedTime)]] -> TreeNode String
callMiningAlgorithm "Inductive Miner" _ parserResult  = inductiveMiner (preProcessingIM parserResult)
callMiningAlgorithm "Inductive Miner - Infrequent" threshold parserResult = inductiveMinerInfrequent (stringToFloat threshold) (preProcessingIM parserResult)
callMiningAlgorithm "IM" _ parserResult  = inductiveMiner (preProcessingIM parserResult)
callMiningAlgorithm "IMF" threshold parserResult = inductiveMinerInfrequent (stringToFloat threshold) (preProcessingIM parserResult)
callMiningAlgorithm "1" _ parserResult  = inductiveMiner (preProcessingIM parserResult)
callMiningAlgorithm "2" threshold parserResult = inductiveMinerInfrequent (stringToFloat threshold) (preProcessingIM parserResult)
callMiningAlgorithm _ _ _ = error "Selected Miner has not been implemented."
 
stringToFloat :: String -> Float
stringToFloat text = read text
