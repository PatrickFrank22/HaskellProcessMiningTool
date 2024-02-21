module MainTool.CallParserAndInductiveMiner ( callParserAndInductiveMiner ) where

import Parser.XesParserXmlConduit
import Parser.XesParserXmlConduitPrefix
import Parser.XesParserHxt
import Parser.CsvParserCassava
import IMfunctions.InductiveMiner
import IMfunctions.InductiveMinerInfrequent
import Helperfunctions.HelperFunctions
import ProcessTreeVisualization.DrawProcessTree (mainDrawProcessTree)
import Data.Time

import qualified Data.Text as T
import Text.XML
import Text.XML.Cursor
import Data.Text (unpack)
import Data.Time
import Data.Time.Format
import Data.Time.LocalTime
import IMfunctions.InductiveMiner
import IMfunctions.InductiveMinerInfrequent
import Helperfunctions.HelperFunctions (preProcessingIM)
import ProcessTreeVisualization.DrawProcessTree
import ParserApp.HelperfunctionsTool.HelperFunctionsTool

import Parser.ParseDateTime


callParserAndInductiveMiner :: T.Text -> T.Text -> FilePath -> T.Text -> T.Text -> T.Text -> T.Text -> IO()
callParserAndInductiveMiner selectedParser selectedMiner filename threshold caseID eventName timestamp = do
    let selectedParser_ = stringValue selectedParser
        selectedMiner_ = stringValue selectedMiner
    

    doc <- Text.XML.readFile def filename
    let cursor = fromDocument doc
        result = parseEventLog cursor
    print (preProcessingIM (convertToString (parseDateTime result "YYYY-MM-DDThh:mm:ss.sTZD")))
    if selectedMiner_ == "IM" 
    then mainDrawProcessTree (inductiveMiner (preProcessingIM (convertToString (parseDateTime result "YYYY-MM-DDThh:mm:ss.sTZD"))))
    else mainDrawProcessTree (inductiveMinerInfrequent (textToFloat (stringValue threshold)) (preProcessingIM (convertToString (parseDateTime result "YYYY-MM-DDThh:mm:ss.sTZD"))))


parseEventLog :: Cursor -> [[(T.Text, T.Text)]]
parseEventLog cursor = map parseTrace (cursor $// element "trace")

parseTrace :: Cursor -> [(T.Text, T.Text)]    
parseTrace cursor = map parseEvent (cursor $/ element "event")

parseEvent :: Cursor -> (T.Text, T.Text)
parseEvent cursor = (T.concat (cursor $// element "string" >=> attributeIs "key" "concept:name" >=> attribute "value"), T.concat (cursor $// element "date" >=> attributeIs "key" "time:timestamp" >=> attribute "value"))

parseDateTime :: [[(T.Text, T.Text)]] -> String -> [[(T.Text, ZonedTime)]]
parseDateTime [] _ = []
parseDateTime (x:xs) dateTimeFormat = (map (\y -> (parseDateTimeHelper y dateTimeFormat)) x) : (parseDateTime xs dateTimeFormat)
  where
    parseDateTimeHelper :: (T.Text, T.Text) -> String -> (T.Text, ZonedTime)
    parseDateTimeHelper (eventName, dateTime) "YYYY-MM-DDThh:mm:ssTZD" = case parseTimeM True defaultTimeLocale 
      "%Y-%m-%dT%H:%M:%S%z" (T.unpack dateTime) of
        Just zonedTime -> (eventName, zonedTime)
        Nothing -> error "Parsing failed"
    parseDateTimeHelper (eventName, dateTime) _ = case parseTimeM True defaultTimeLocale 
      "%Y-%m-%dT%H:%M:%S.%f%z" (T.unpack dateTime) of
        Just zonedTime -> (eventName, zonedTime)
        Nothing -> error "Parsing failed"


    --parserResult <- callParser selectedParser
    --let log = (preProcessingIM parserResult)
    --if log == [] then putStrLn "Parsing error!" else putStrLn ""

    --let minerResult = callMiningAlgorithm selectedMiner threshold parserResult
    --mainDrawProcessTree minerResult


callParser :: String -> IO [[(String, ZonedTime)]]
callParser "XES xml-conduit" = xesParserXmlConduit
callParser "XES xml-conduit prefix variant" = xesParserXmlConduitPrefix
callParser "XES hxt" = xesParserHxt
callParser "CSV" = csvParser
callParser _ = error "Selected Parser has not been implemented."

callMiningAlgorithm :: String -> String -> [[(String, ZonedTime)]] -> TreeNode String
callMiningAlgorithm "I" _ parserResult  = inductiveMiner (preProcessingIM parserResult)
callMiningAlgorithm "f" threshold parserResult = inductiveMinerInfrequent (stringToFloat threshold) (preProcessingIM parserResult)
callMiningAlgorithm "Inductive Miner" _ parserResult  = inductiveMiner (preProcessingIM parserResult)
callMiningAlgorithm "Inductive Miner - Infrequent" threshold parserResult = inductiveMinerInfrequent (stringToFloat threshold) (preProcessingIM parserResult)
callMiningAlgorithm _ _ _ = error "Selected Miner has not been implemented."
 
stringToFloat :: String -> Float
stringToFloat text = read text
