{-# LANGUAGE OverloadedStrings #-}

module ParserApp.HelperfunctionsTool.HelperFunctionsTool (
    convertToString,
    stringValue
) where

import qualified Data.Text as T
import Data.Text (unpack)
import Data.Time


stringValue :: T.Text -> String
stringValue textValue = T.unpack textValue

convertToString :: [[(T.Text, ZonedTime)]] -> [[(String, ZonedTime)]]
convertToString [] = []
convertToString (l:ls) = map (\(x,timestamp) -> (stringValue x,timestamp)) l : convertToString ls

