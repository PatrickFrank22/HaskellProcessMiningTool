{-# LANGUAGE OverloadedStrings #-}

{- 
--- This implementation is based on: ---
1. Michael Snoyman and Aristid Breitkreuz. xml-conduit: Pure-Haskell utilities for dealing with XML with the conduit package., 2023. https://hackage.haskell.org/ package/xml-conduit. Accessed on 16 Feb. 2024.
2. Michael Snoyman. Developing Web Apps with Haskell and Yesod - Safety-Driven Web Development, Second Edition. O’Reilly, 2015. ISBN 978-1-491-91559-2. http://www.oreilly.de/catalog/9781491915592/index.html. Copyright (c) 2015, Michael Snoyman. All rights reserved.
-}

module ParserApp.XesParserXmlConduitPrefix ( xesParserXmlConduitPrefix ) where

import qualified Data.Text as T
import Text.XML
import Text.XML.Cursor
import Data.Text (unpack)
import Data.Time
import Parser.ParseDateTime
import Helperfunctions.HelperFunctions (preProcessingIM)
import ProcessTreeVisualization.DrawProcessTree
import ParserApp.HelperfunctionsTool.HelperFunctionsTool
import MainConsole.MainConsole


xesParserXmlConduitPrefix :: FilePath -> T.Text -> T.Text -> IO()
xesParserXmlConduitPrefix fileName selectedMiner threshold = do

    doc <- Text.XML.readFile def fileName
    let cursor = fromDocument doc
        parserResult = parseEventLog cursor
        log = preProcessingIM parserResult

    if log == [] then putStrLn "Parsing error!" else putStrLn ""

    let minerResult = callMiningAlgorithm (stringValue selectedMiner) (stringValue threshold) parserResult
    mainDrawProcessTree minerResult


parseEventLog :: Cursor -> [[(String, ZonedTime)]]
parseEventLog cursor = map parseTrace (cursor $// element "{http://www.xes-standard.org/}trace")

parseTrace :: Cursor -> [(String, ZonedTime)]    
parseTrace cursor = map parseEvent (cursor $/ element "{http://www.xes-standard.org/}event")

parseEvent :: Cursor -> (String, ZonedTime)
parseEvent cursor = (unpack (T.concat (cursor $// element "{http://www.xes-standard.org/}string" >=> 
  attributeIs "key" "concept:name" >=> attribute "value")), parseDateTime (unpack (T.concat 
  (cursor $// element "{http://www.xes-standard.org/}date" >=> attributeIs "key" "time:timestamp" >=> attribute "value"))))
