{-
-- EPITECH PROJECT, 2024
-- compressor
-- File description:
-- ArgsParser
-}

module ArgsParser
    ( parseArgs
    , allowedFormats
    , Options(..)
    ) where

import Options.Applicative
import System.Exit (exitWith, ExitCode(..))
import Control.Exception (catch, SomeException, throwIO, fromException)
import Data.List (intercalate)
import System.IO (withFile, hGetChar, IOMode(ReadMode))

data Options = Options
    { optInput :: !String
    , optFormat :: !String
    , optOutput :: Maybe String
    , optInputFormat :: Maybe String
    } deriving (Show, Eq)

verifyFormatOutput :: Options -> IO Options
verifyFormatOutput options = case optOutput options of
    Just output -> if getExt output == optFormat options
        then return options
        else exitWith (ExitFailure 84)
    Nothing -> return options

verifyFormatInput :: Options -> IO Options
verifyFormatInput options = case optInputFormat options of
    Just format -> if getExt (optInput options) == format
        then return options
        else exitWith (ExitFailure 84)
    Nothing -> return options

addFormatToOutput :: Options -> IO Options
addFormatToOutput options = case optOutput options of
    Just output -> return options { optOutput = Just (output ++ "."
        ++ optFormat options) }
    Nothing -> return options

checkOutput :: Options -> IO Options
checkOutput options = case optOutput options of
    Just output -> if '.' `elem` output
        then return options
        else addFormatToOutput options
    Nothing -> return options

renameMarkdownInput :: Options -> IO Options
renameMarkdownInput options = case optInputFormat options of
    Just format -> if format == "markdown"
        then return options { optInputFormat = Just "md" }
        else return options
    Nothing -> return options

renameMarkdownOutput :: Options -> IO Options
renameMarkdownOutput options = if optFormat options == "markdown"
    then return options { optFormat = "md" }
    else return options

renameMarkdownPathOutput :: Options -> IO Options
renameMarkdownPathOutput options = case optOutput options of
    Just out -> if getExt out == "markdown"
        then return options{optOutput=Just (take(length out - 8)out ++ "md")}
        else return options
    Nothing -> return options

getExtInput :: Options -> IO Options
getExtInput options = case optInputFormat options of
        Just format -> return $ handleJust format options
        Nothing -> return $ handleNothing options

handleJust :: String -> Options -> Options
handleJust format options
    | isDotInInput options = options
    | isFormatMarkdown format = appendMarkdownToInput options
    | otherwise = appendFormatToInput format options

isDotInInput :: Options -> Bool
isDotInInput options = '.' `elem` optInput options

isFormatMarkdown :: String -> Bool
isFormatMarkdown format = format == "markdown"

appendMarkdownToInput :: Options -> Options
appendMarkdownToInput options = options {
    optInput = optInput options ++ ".md" }

appendFormatToInput :: String -> Options -> Options
appendFormatToInput format options = options {
    optInput = optInput options ++ "." ++ format }

handleNothing :: Options -> Options
handleNothing options = options

checkValidInput :: Options -> IO Options
checkValidInput options = case validateFileExtension (optInput options) of
    Left _ -> exitWith (ExitFailure 84)
    Right _ -> return options

parseArgs :: IO Options
parseArgs = catch (execParser opts) errorHandler >>= getExtInput
    >>= checkFormat
    >>= checkOutput >>= verifyFormatOutput >>= renameMarkdownOutput >>=
        renameMarkdownInput >>= verifyFormatInput
        >>= renameMarkdownPathOutput >>= checkValidInput

errorHandler :: SomeException -> IO Options
errorHandler e = case fromException e of
    Just ExitSuccess -> throwIO e
    _ -> exitWith (ExitFailure 84)


checkFormat :: Options -> IO Options
checkFormat options = case optInputFormat options of
    Just _  -> return options
    Nothing -> do
        format <- getFormat (optInput options)
        return options { optInputFormat = Just format }

opts :: ParserInfo Options
opts = info (parseOptions <**> helper)
    fullDesc

getFormat :: FilePath -> IO String
getFormat filePath = withFile filePath ReadMode $ \handle -> do
    firstChar <- hGetChar handle
    case firstChar of
        '{' -> return "json"
        '<' -> return "xml"
        '-' -> return "markdown"
        _   -> error "Unknown format"

allowedFormats :: [String]
allowedFormats = ["xml", "json", "markdown", "md"]

allowedExtensions :: [String]
allowedExtensions = ["xml", "json", "md", "markdown"]

getExt :: FilePath -> String
getExt = reverse . takeWhile (/= '.') . reverse

validateFileExtension :: String -> Either String String
validateFileExtension arg =
    if getExt arg `notElem` allowedExtensions
        then Left "Invalid file extension."
        else Right arg

validateFormat :: String -> Either String String
validateFormat arg =
    if arg `notElem` allowedFormats
        then Left "Invalid format."
        else Right arg

outputFormatHelp :: String
outputFormatHelp = "output format (" ++ intercalate ", " allowedFormats ++ ")"

inputFormatHelp :: String
inputFormatHelp = "input format (xml, json, markdown, md)"

parseOptions :: Parser Options
parseOptions = Options
    <$> strOption
        (short 'i' <> metavar "ifile" <> help "path of the file to convert")
    <*> option (eitherReader validateFormat)
        (short 'f' <> metavar "oformat" <> help (outputFormatHelp)
        <> completeWith allowedFormats)
    <*> optional (strOption
        (short 'o' <> metavar "ofile" <> help "path of the output file"))
    <*> optional (option (eitherReader validateFormat)
        (short 'e' <> metavar "iformat" <> help (inputFormatHelp)))