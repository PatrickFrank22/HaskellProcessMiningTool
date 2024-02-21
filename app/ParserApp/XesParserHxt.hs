{-# LANGUAGE Arrows #-}

{-  
--- This implementation is based on: ---
1. Uwe Schmidt, Martin Schmidt, Torben Kuseler. hxt: A collection of tools for processing XML with Haskell., 2021. https://hackage.haskell.org/package/hxt. Accessed on 16 Feb. 2024.
2. HXT/Practical/Simple1, 2011. https://wiki.haskell.org/HXT/Practical/Simple1. Accessed on 16 Feb. 2024.
3. A Gentle Introduction to the Haskell XML Toolbox, 2016. https://wiki.haskell.org/HXT. Accessed on 16 Feb. 2024.
-}

module ParserApp.XesParserHxt ( xesParserHxt ) where

import Text.XML.HXT.Core
import Data.Time
import Parser.ParseDateTime
import qualified Data.Text as T
import Helperfunctions.HelperFunctions (preProcessingIM)
import ProcessTreeVisualization.DrawProcessTree
import ParserApp.HelperfunctionsTool.HelperFunctionsTool
import MainConsole.MainConsole


xesParserHxt :: FilePath -> T.Text -> T.Text -> IO()
xesParserHxt fileName selectedMiner threshold = do

    parserResult <- runX $ readDocument [] fileName >>> parseEventLog
    let log = preProcessingIM parserResult

    if log == [] then putStrLn "Parsing error!" else putStrLn ""

    let minerResult = callMiningAlgorithm (stringValue selectedMiner) (stringValue threshold) parserResult
    mainDrawProcessTree minerResult


parseEventLog :: IOSArrow XmlTree [(String, ZonedTime)]
parseEventLog = deep (isElem >>> hasName "trace" >>> parseTrace)

parseTrace :: IOSArrow XmlTree [(String, ZonedTime)]
parseTrace = listA (deep (isElem >>> hasName "event" >>> parseEvent))

parseEvent :: IOSArrow XmlTree (String, ZonedTime)
parseEvent = proc event -> do
    eventName <- deep (isElem >>> hasName "string" >>> 
      hasAttrValue "key" (== "concept:name")) >>> getAttrValue "value" -< event
    timestamp <- deep (isElem >>> hasName "date" >>> 
      hasAttrValue "key" (== "time:timestamp")) >>> getAttrValue "value" -< event
    returnA -< (eventName, parseDateTime timestamp)
