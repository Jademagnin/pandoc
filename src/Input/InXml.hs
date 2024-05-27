{-
-- EPITECH PROJECT, 2024
-- compressor
-- File description:
-- ArgsParser
-}

module Input.InXml
    (
        parseXmlObject,
        wsXML,
        verifyXml
    ) where

import Control.Applicative (Alternative(..), some, many)
import Data.Char (isSpace)
import Input.Parser
import Types (Header(..), Content(..), Item(..), Document(..))

verifyQuotes :: String -> Int -> Bool
verifyQuotes [] 0 = True
verifyQuotes [] _ = False
verifyQuotes (x:xs) n = case x of
    '"' -> if n == 0
           then verifyQuotes xs (n + 1)
           else verifyQuotes xs (n - 1)
    _   -> verifyQuotes xs n

verifyXml :: String -> Bool
verifyXml str = verifyQuotes str 0

parseXmlElement :: String -> Parser String
parseXmlElement name = do
    _ <- string $ "<" ++ name ++ ">"
    wsXML
    content <- parseXmlText
    _ <- string $ "</" ++ name ++ ">"
    return content

parseXmlElement' :: String -> Parser String
parseXmlElement' name = do
    _ <- string $ "<" ++ name ++ ">"
    wsXML
    content <- parseXmlText
    _ <- string' $ "</" ++ name ++ ">"
    return content

parseOptionalXmlElement :: Parser String -> Parser (Maybe String)
parseOptionalXmlElement p = (Just <$> p) <|> pure Nothing

parseXmlObject :: Parser Document
parseXmlObject = do
    _ <- string "<document>"
    wsXML
    header <- parseXmlHeader
    content <- parseXmlContent
    _ <- string "</document>"
    return $ Document header content

parseXmlAttribute :: String -> Parser String
parseXmlAttribute attr = do
    _ <- string $ attr ++ "=\""
    value <- manyUntill (satisfy (/= '"')) (char '"')
    return value

parseAuthorAndDate :: Parser (Maybe String, Maybe String)
parseAuthorAndDate = do
    auth <- parseOptionalXmlElement $ parseXmlElement "author"
    dat <- parseOptionalXmlElement $ parseXmlElement "date"
    return (auth, dat)

parseXmlHeader :: Parser Header
parseXmlHeader = do
    _ <- string "<header"
    wsXML
    titleValue <- parseXmlAttribute "title"
    _ <- string ">"
    wsXML
    (auth, dat) <- parseAuthorAndDate
    _ <- string "</header>"
    return $ Header titleValue auth dat

parseXmlContent :: Parser [Content]
parseXmlContent = do
    _ <- string "<body>"
    wsXML
    parts <- many (parseParagraph <|> parseSection <|> parseCodeBlock
        <|> parseList)
    _ <- string "</body>"
    return parts

parseSection :: Parser Content
parseSection = do
    _ <- string "<section"
    wsXML
    titleValue <- parseXmlAttribute "title"
    _ <- string ">"
    wsXML
    contents <- many (parseParagraph <|> parseSection <|> parseCodeBlock
        <|> parseList)
    _ <- string "</section>"
    return $ Section (titleValue, contents)

parseParagraph :: Parser Content
parseParagraph = parsexmlelement "paragraph" where
    parsexmlelement name = do
        _ <- string $ "<" ++ name ++ ">"
        content <- many (parseXmlParagraphContent)
        _ <- string $ "</" ++ name ++ ">"
        return $ Paragraph content

parseParagraphBis :: Parser Content
parseParagraphBis = do
    content <- parseTextElement
    return $ Paragraph [content]

parseXmlParagraphContent :: Parser Content
parseXmlParagraphContent = parseParagraph <|> parseBold <|> parseItalic <|>
    parseCode <|> parseLink <|> parseImage <|> parseList <|> parseCodeBlock
    <|> parseSection <|> parseTextElement

parseTextElement :: Parser Content
parseTextElement = TextElement <$> parseXmlText

parseBold :: Parser Content
parseBold = Bold <$> parseXmlElement' "bold"

parseItalic :: Parser Content
parseItalic = Italic <$> parseXmlElement' "italic"

parseCode :: Parser Content
parseCode = Code <$> parseXmlElement' "code"

parseLink :: Parser Content
parseLink = do
    _ <- string "<link"
    wsXML
    url <- parseXmlAttribute "url"
    _ <- string ">"
    content <- parseParagraphBis
    _ <- string "</link>"
    return $ Link (url, content)

parseImage :: Parser Content
parseImage = do
    _ <- string "<image"
    wsXML
    url <- parseXmlAttribute "url"
    _ <- string ">"
    alternateText <- parseParagraphBis
    _ <- string "</image>"
    return $ Image (url, alternateText)

parseCodeBlock :: Parser Content
parseCodeBlock = do
    _ <- string "<codeblock>"
    wsXML
    content <- parseParagraph
    _ <- string "</codeblock>"
    return $ CodeBlock content

parseList :: Parser Content
parseList = do
    _ <- string "<list>"
    wsXML
    items <- parseListItem
    _ <- string "</list>"
    return $ List [items]

parseListItem :: Parser Item
parseListItem = do
    content <- many parseParagraph
    return $ Item content

parseXmlText :: Parser String
parseXmlText = do
    content <- some (satisfy (/= '<'))
    let filteredContent = filter (\c -> c /= '\n' && c /= '\t') content
    return filteredContent

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string s = traverse char s <* wsXML

string' :: String -> Parser String
string' s = traverse char s

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
    (x:xs) | p x -> Just (x, xs)
    _ -> Nothing

wsXML :: Parser ()
wsXML = many (satisfy isSpace) *> pure ()

manyUntill :: Parser Char -> Parser Char -> Parser String
manyUntill p end = loop ""
  where
    loop acc = (do
        _ <- end
        return acc)
      <|> (do
        x <- p
        loop (acc ++ [x]))
