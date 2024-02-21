{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-
--- This implementation is based on: ---
Michael Snoyman. Developing Web Apps with Haskell and Yesod - Safety-Driven Web Development, Second Edition. O’Reilly, 2015. ISBN 978-1-491-91559-2. http://www.oreilly.de/catalog/9781491915592/index.html. Copyright (c) 2015, Michael Snoyman. All rights reserved.
-}

module MainTool.MainTool where

import           Control.Applicative ((<$>), (<*>))
import           Yesod
import           Yesod.Form.Jquery


import ParserApp.XesParserXmlConduit
import ParserApp.XesParserXmlConduitPrefix
import ParserApp.XesParserHxt
import ParserApp.CsvParserCassava
import qualified Data.Text as T
import Data.Text (Text, unpack)
import Data.List (isSuffixOf)


data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/prompt PromptR POST
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodJquery App


data Prompt = Prompt
    { promptAlgorithm :: Text
    , promptParser :: Text
    , promptFileName :: Text
    , promptThreshold :: Maybe Text
    , promptCaseID :: Maybe Text
    , promptEventName :: Maybe Text
    , promptTimestamp :: Maybe Text
    }


processPrompt :: Prompt -> IO Text
processPrompt prompt = do
    let selectedAlgorithm = promptAlgorithm prompt
        noiseThreshold = promptThreshold prompt
        filename = promptFileName prompt
        caseID = promptCaseID prompt
        eventName = promptEventName prompt
        timestamp = promptTimestamp prompt
    case (promptParser prompt) of
        "hxt" -> do 
            case noiseThreshold of
                Nothing -> do 
                    xesParserHxt ("logs/" <> unpack filename) selectedAlgorithm  "0.0"
                    if (checkSuffix ".xes" filename) 
                    then (if selectedAlgorithm == "IMF" then (return "Error. Noise threshold is missing.") else (return "Success."))
                    else (return fileFormatError)

                Just valueNoiseThreshold -> do 
                    xesParserHxt ("logs/" <> unpack filename) selectedAlgorithm  valueNoiseThreshold
                    if (checkSuffix ".xes" filename) then (return "Success.") else (return fileFormatError)
        "conduit" -> do 
            case noiseThreshold of
                Nothing -> do 
                    xesParserXmlConduit ("logs/" <> unpack filename) selectedAlgorithm  "0.0"
                    if (checkSuffix ".xes" filename) 
                    then (if selectedAlgorithm == "IMF" then (return "Error. Noise threshold is missing.") else (return "Success."))
                    else (return fileFormatError)

                Just valueNoiseThreshold -> do 
                    xesParserXmlConduit ("logs/" <> unpack filename) selectedAlgorithm  valueNoiseThreshold
                    if (checkSuffix ".xes" filename) then (return "Success.") else (return fileFormatError)
        "conduitPrefix" -> do 
            case noiseThreshold of
                Nothing -> do 
                    xesParserXmlConduitPrefix ("logs/" <> unpack filename) selectedAlgorithm  "0.0"
                    if (checkSuffix ".xes" filename) 
                    then (if selectedAlgorithm == "IMF" then (return "Error. Noise threshold is missing.") else (return "Success."))
                    else (return fileFormatError)

                Just valueNoiseThreshold -> do 
                    xesParserXmlConduitPrefix ("logs/" <> unpack filename) selectedAlgorithm  valueNoiseThreshold
                    if (checkSuffix ".xes" filename) then (return "Success.") else (return fileFormatError)
        _ -> do 
            case noiseThreshold of
                Nothing -> do 
                    case caseID of
                        Nothing -> do
                            if (checkSuffix ".csv" filename) then (return "Error. Case ID identifier is missing.") else (return fileFormatError)
                        Just caseID_ -> do
                            case eventName of
                                Nothing -> do
                                    if (checkSuffix ".csv" filename) then (return "Error. Event name identifier is missing.") else (return fileFormatError)
                                Just eventName_ -> do
                                    case timestamp of
                                        Nothing -> do
                                            if (checkSuffix ".csv" filename) then (return "Error. Timestamp identifier is missing.") else (return fileFormatError)
                                        Just timestamp_ -> do
                                            csvParser ("logs/" <> unpack filename) selectedAlgorithm  "0.0" caseID_ eventName_ timestamp_
                                            if (checkSuffix ".csv" filename) 
                                            then (if selectedAlgorithm == "IMF" then (return "Error. Noise threshold is missing.") else (return "Success."))
                                            else (return fileFormatError)

                Just valueNoiseThreshold -> do 
                    case caseID of
                        Nothing -> do
                            if (checkSuffix ".csv" filename) then (return "Error. Case ID identifier is missing.") else (return fileFormatError)
                        Just caseID_ -> do
                            case eventName of
                                Nothing -> do
                                    if (checkSuffix ".csv" filename) then (return "Error. Event name identifier is missing.") else (return fileFormatError)
                                Just eventName_ -> do
                                    case timestamp of
                                        Nothing -> do
                                            if (checkSuffix ".csv" filename) then (return "Error. Timestamp identifier is missing.") else (return fileFormatError)
                                        Just timestamp_ -> do
                                            csvParser ("logs/" <> unpack filename) selectedAlgorithm  valueNoiseThreshold caseID_ eventName_ timestamp_
                                            if (checkSuffix ".csv" filename) then (return "Success.") else (return fileFormatError)


checkSuffix :: String -> Text -> Bool
checkSuffix suffix word = isSuffixOf suffix (T.unpack word)

fileFormatError :: Text
fileFormatError = "Error. Wrong file format for the selected parser."


promptForm :: Html -> MForm Handler (FormResult Prompt, Widget)
promptForm = renderDivs $ Prompt
    <$> areq (selectFieldList [("Inductive Miner" :: Text, "IM"),("Inductive Miner - Infrequent", "IMF")]) "Algorithm: " Nothing
    <*> areq (selectFieldList [("CSV cassava parser" :: Text, "cassava"),("XES xml-conduit parser", "conduit"),
                                 ("XES xml-conduit parser prefix version", "conduitPrefix"),("XES hxt parser", "hxt")]) "Parser: " Nothing
    <*> areq textField "Filename (from the 'log' folder): " Nothing
    <*> aopt textField "Noise threshold in between 0.0 and 1.0 (if the Inductive Miner - Infrequent is selected): " Nothing
    <*> aopt textField "Case ID column identifier (if the CSV parser is selected): " Nothing
    <*> aopt textField "Event name column identifier (if the CSV parser is selected): " Nothing
    <*> aopt textField "Timestamp column identifier (if the CSV parser is selected): " Nothing


-- GET handler
getHomeR :: Handler Html
getHomeR = do
    -- Generate the form to be displayed
    (widget, enctype) <- generateFormPost promptForm
    defaultLayout
        [whamlet|
            <style> body { font-family: Arial, sans-serif; font-size: 16px; line-height: 1.5; }
            <h1> Haskell & Process Mining
            <p>
                Please select the algorithms you would like to use and enter the required information:
            <form method=post action=@{PromptR} enctype=#{enctype}>
                  ^{widget}
                <p>If all information is complete and correct, press 'Submit' to calculate the process tree.
                <button>Submit
        |]


-- POST handler
postPromptR :: Handler Html
postPromptR = do
    ((result, widget), enctype) <- runFormPost promptForm
    case result of 
        FormSuccess prompt -> do
               parsingResult <- processPromptHandler prompt
               defaultLayout [whamlet|
                   <style> body { font-family: Arial, sans-serif; font-size: 16px; line-height: 1.5; }
                   <h2>Resulting Process Tree
                   <img src="Result/resultingProcessTree.png"> 
                   <p>#{parsingResult}
                   <button><a href=@{HomeR}>Go back|]


        _ -> defaultLayout
            [whamlet|
                <style> body { font-family: Arial, sans-serif; font-size: 16px; line-height: 1.5; }
                <p>Invalid input, let's try again.
                <form method=post action=@{PromptR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]


mainApp :: IO ()
mainApp = warp 3000 App

processPromptHandler :: Prompt -> Handler Html
processPromptHandler prompt = do
    parserResult <- liftIO $ processPrompt prompt
    return $ toHtml parserResult
