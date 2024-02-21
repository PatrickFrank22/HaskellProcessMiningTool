{-# LANGUAGE OverloadedStrings #-}

{-  
--- This implementation is based on: ---
Johan Tibell. cassava: A CSV parsing and encoding library, 2023. https://hackage.haskell.org/package/cassava. Accessed on 16 Feb. 2024.
-}

module Parser.CsvParserCassava ( csvParser ) where

import Data.Csv
import Data.ByteString (split)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List (groupBy, sortOn)
import Control.Applicative 
import Data.Time
import Parser.ParseDateTime
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Helperfunctions.HelperFunctions (errorSafeHead)


data Event = Event {trace :: !String, name :: !String, timestamp :: !String}

instance FromNamedRecord Event where
    parseNamedRecord r = Event <$> r .: "caseID" <*> r .: "activity" <*> r .: "timestamp" 

csvParser :: IO [[(String, ZonedTime)]]
csvParser = do
    putStrLn "Please enter the filename (without \"\")."
    fileName <- getLine
    putStrLn "Please enter the caseID identifier (without \"\")."
    caseIDName <- getLine
    putStrLn "Please enter the activity / event name identifier  (without \"\")."
    eventName <- getLine
    putStrLn "Please enter the timestamp identifier (without \"\")."
    timestampName <- getLine

    csvData <- BL.readFile ("logs/" ++ fileName)
    csvDataRenamed <- renameFirstRow csvData (convertStringToBLBS caseIDName) 
      (convertStringToBLBS eventName) (convertStringToBLBS timestampName)

    case decodeByName csvDataRenamed of
        Left err -> return [[(err, errZonedTime)]]
        Right (_, v) -> do
            let eventList = V.foldr collectEvents [] v
                parserResult = sortEventsByTimestamp (collectTraces (sortOn errorSafeHead eventList))
            return parserResult

collectEvents :: Event -> [[String]] -> [[String]]
collectEvents e res =
    [trace e, name e, timestamp e] : res

collectTraces :: [[String]] -> [[(String, ZonedTime)]]
collectTraces sortedEventList = map getEventInfo $ groupByTrace sortedEventList
  where
    groupByTrace = groupBy (\x y -> errorSafeHead x == errorSafeHead y)
    getEventInfo trace = map (\lst -> (lst !! 1, parseDateTime (lst !! 2))) trace

sortEventsByTimestamp :: [[(String, ZonedTime)]] -> [[(String, ZonedTime)]]
sortEventsByTimestamp [] = []
sortEventsByTimestamp (x:xs) = (sortOn (\(_, zt) -> zonedTimeToUTC zt) x) : 
  sortEventsByTimestamp xs

renameFirstRow :: BL.ByteString -> BL.ByteString -> BL.ByteString -> BL.ByteString -> 
  IO BL.ByteString
renameFirstRow contents caseIDName eventName timestampName = do
  let getRows = BL.split 10 contents -- fromEnum '\n' == 10
      getFirstRowNames = BL.split 44 (errorSafeHead getRows) -- fromEnum ',' == 44
      getFirstRowNames' = replaceName getFirstRowNames caseIDName "caseID"
      getFirstRowNames'' = replaceName getFirstRowNames' eventName "activity"
      getFirstRowNames''' = replaceName getFirstRowNames'' timestampName "timestamp"
      rowsMerged = BL.concat (mergeRows (removeFirstRow getRows))
      result = BL.concat ((mergeNames getFirstRowNames''') ++ ["\n", rowsMerged])
  return result
    where
      replaceName :: [BL.ByteString] -> BL.ByteString -> BL.ByteString -> [BL.ByteString]
      replaceName [] _ _ = []
      replaceName (x:xs) oldName newName | x == oldName = newName : 
                                             (replaceName xs oldName newName)
                                         | otherwise = x : (replaceName xs oldName newName)

      mergeNames :: [BL.ByteString] -> [BL.ByteString]
      mergeNames [] = []
      mergeNames (x:xs) = x : (",") : (mergeNames xs)

      mergeRows :: [BL.ByteString] -> [BL.ByteString]
      mergeRows [] = []
      mergeRows (x:xs) = x : ("\n") : (mergeRows xs)

      removeFirstRow :: [BL.ByteString] -> [BL.ByteString]
      removeFirstRow (y:ys) = ys

convertStringToBLBS :: String -> BL.ByteString
convertStringToBLBS str = BL.fromStrict (TE.encodeUtf8 (T.pack str))

errZonedTime :: ZonedTime
errZonedTime = 
  ZonedTime (LocalTime (fromGregorian 0000 0 0) (TimeOfDay 0 0 0)) (TimeZone 0 False "UTC")