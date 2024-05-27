{-
-- EPITECH PROJECT, 2024
-- compressor
-- File description:
-- Main
-}

module Main (main) where

import Lib

import Control.Monad (when)
import Control.Exception
import System.Exit (exitWith, ExitCode(..), exitSuccess)

parseJson :: String -> Either String Document
parseJson content =
    let parser = ws *> parseJsonObject <* ws
        parsedContent = runParser parser content in
    case parsedContent of
        Just (document, _) -> Right document
        Nothing -> Left "Failed to parse Json file"

parseJsonContent :: String -> Either String Document
parseJsonContent content = parseJson content

formatJsonContent :: Options -> Document -> String
formatJsonContent opts document =
    case optFormat opts of
        "xml" -> prettyPrint document Xml
        "md" -> prettyPrint document Markdown
        _ -> prettyPrintJsonDocument document

parseAndFormatJson :: Options -> String -> Either String String
parseAndFormatJson opts content =
    case parseJsonContent content of
        Left errorMessage -> Left errorMessage
        Right document -> Right (formatJsonContent opts document)

outputFormattedJson :: Options -> Either String String -> String -> IO ()
outputFormattedJson _ (Left errorMessage) _ = putStrLn errorMessage >>
    exitWith (ExitFailure 84)
outputFormattedJson opts (Right formattedContent) originalContent =
    let outputContent = if optFormat opts == "json"
                        then originalContent
                        else formattedContent
    in case optOutput opts of
        Nothing -> putStr outputContent
        Just outputPath -> writeFile outputPath outputContent


processJson :: Options -> String -> IO ()
processJson opts content = 
    let formattedContent = parseAndFormatJson opts content in
    outputFormattedJson opts formattedContent content

verifyXmlContent :: String -> Either String String
verifyXmlContent content =
    if verifyXml content
        then Right content
        else Left "Xml file is incorrect"

parseXmlContent :: String -> Either String Document
parseXmlContent content =
    let parser = wsXML *> parseXmlObject <* wsXML
        parsedContent = runParser parser content in
    case parsedContent of
        Just (document, _) -> Right document
        Nothing -> Left "Failed to parse XML file"

formatParsedXmlContent :: Options -> Document -> String
formatParsedXmlContent opts document =
    case optFormat opts of
        "json" -> prettyPrint document Json
        "md" -> prettyPrint document Markdown
        _ -> prettyPrintXMLDocument document

parseAndFormatXml :: Options -> String -> Either String String
parseAndFormatXml opts content =
    case verifyXmlContent content of
        Left errorMessage -> Left errorMessage
        Right validContent ->
            case parseXmlContent validContent of
                Left errorMessage -> Left errorMessage
                Right document -> Right (formatParsedXmlContent opts document)

outputFormattedXml :: Options -> Either String String -> String -> IO ()
outputFormattedXml _ (Left errorMessage) _ = putStrLn errorMessage >>
    exitWith (ExitFailure 84)
outputFormattedXml opts (Right formattedContent) originalContent =
    let outputContent = if optFormat opts == "xml"
                        then originalContent
                        else formattedContent
    in case optOutput opts of
        Nothing -> putStr outputContent
        Just outputPath -> writeFile outputPath outputContent

processXml :: Options -> String -> IO ()
processXml opts content = 
    let formattedContent = parseAndFormatXml opts content in
    outputFormattedXml opts formattedContent content

outputFormattedMd :: Options -> String -> IO ()
outputFormattedMd opts originalContent =
    let outputContent = originalContent
    in case optOutput opts of
        Nothing -> putStr outputContent
        Just outputPath -> writeFile outputPath outputContent

processMd :: Options -> String -> IO ()
processMd opts content = outputFormattedMd opts content

parseAndRead :: IO (Options, String)
parseAndRead = do
    opts <- parseArgs
    content <- readDocument opts
    return (opts, content)

processContent :: (Options, String) -> IO ()
processContent (opts, content) = 
    when (optInputFormat opts == Just "json") (processJson opts content) >>
    when (optInputFormat opts == Just "xml") (processXml opts content) >>
    when (optInputFormat opts == Just "md" ||
    optInputFormat opts == Just "markdown") (processMd opts content) >>
    exitSuccess

main :: IO ()
main = catch (parseAndRead >>= processContent) handler
    where
        handler :: SomeException -> IO ()
        handler e = case fromException e of
                Just ExitSuccess -> return ()
                _ -> do
                        putStrLn "Error: An error occurred"
                        exitWith (ExitFailure 84)