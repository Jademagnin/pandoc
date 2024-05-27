{-
-- EPITECH PROJECT, 2024
-- compressor
-- File description:
-- ArgsParser
-}

module Input.InJson
    (
        parseJsonObject,
        ws
    ) where

import Control.Applicative (Alternative(..), many, optional)
import Data.Char (isSpace)
import Input.Parser
import Types (Header(..), Content(..), Item(..), Document(..) )

optionalField :: Parser String -> Parser (Maybe String)
optionalField p = (Just <$> p) <|> pure Nothing

parseHeader :: Parser Header
parseHeader = do
    _ <- string "\"header\"" *> ws *> char ':' *> ws *> char '{' *> ws
    htitle <- jsonPair "title"
    hauthor <- optionalField (char ',' *> ws *> jsonPair "author")
    hdate <- optionalField ((char ',' *> ws *> jsonPair "date") <|> pure "")
    _ <- ws *> char '}'
    return Header {title = htitle, author = hauthor, date = hdate}

char :: Char -> Parser Char
char c = Parser $ \input -> case input of
    (x:xs) | x == c -> Just (c, xs)
    _ -> Nothing

string :: String -> Parser String
string [] = pure []
string (c:cs) = do
    _ <- char c
    _ <- string cs
    return (c:cs)

ws :: Parser ()
ws = many (satisfy isSpace) *> pure ()

jString :: Parser String
jString = do
    _ <- char '"'
    content <- many (satisfy (/= '"'))
    _ <- char '"'
    return content

jsonPair :: String -> Parser String
jsonPair key = do
    _ <- string "\"" *> string key *> string "\":"
    ws
    value <- jString
    ws
    return value

parseJsonTextElem :: Parser Content
parseJsonTextElem = do
    _ <- ws
    text <- jString
    _ <- ws <* optional (char ',' *> ws)
    return $ TextElement text

parseJsonParagraph :: Parser Content
parseJsonParagraph = do
    _ <- ws *> char '['
    texts <- many (parseJsonTextElem <|> parseJsonEnsemble)
    _ <- char ']' <* ws <* optional (char ',' *> ws)
    return $ Paragraph (texts)

parseJsonCB :: Parser Content
parseJsonCB = do
    _ <- string "\"codeblock\"" *> ws *> char ':' *> ws *> char '['
    content <- parseJsonParagraph <|> parseJsonEnsemble <|> parseJsonTextElem
    _ <- ws <* char ']'
    return $ CodeBlock content

parseJsonSection :: Parser Content
parseJsonSection = do
    _ <- string "\"section\"" *> ws *> char ':' *> ws *> char '{' *> ws
    stitle <- jsonPair "title" <* char ',' <* ws
    _ <- string "\"content\"" *> ws *> char ':' *> ws *> char '['
    content <- many (parseJsonParagraph <|> parseJsonEnsemble
        <|> parseJsonTextElem)
    _ <- ws *> char ']'
    _ <- ws <* char '}' <* ws <* optional (char ',' *> ws)
    return $ Section (stitle, content)

parseJsonItalic :: Parser Content
parseJsonItalic = do
    _ <- ws
    italic <- jsonPair "italic"
    return $ Italic italic

parseJsonBold :: Parser Content
parseJsonBold = do
    _ <- ws
    bold <- jsonPair "bold"
    return $ Bold bold

parseJsonCode :: Parser Content
parseJsonCode = do
    _ <- ws
    code <- jsonPair "code"
    return $ Code code


parseJsonEnsemble :: Parser Content
parseJsonEnsemble = do
    _ <- ws *> char '{' *> ws
    content <- parseJsonSection <|> parseJsonParagraph <|> parseJsonCB
        <|> parseJsonItalic <|> parseJsonBold <|>
        parseJsonCode <|> parseJsonList <|> parseJsonLink <|> parseJsonImage
    _ <- ws *> char '}' <* ws <* optional (char ',' *> ws)
    return content

parseJsonList :: Parser Content
parseJsonList = do
    _ <- string "\"list\"" *> ws *> char ':' *> ws *> char '[' *> ws
    content <- parseJsonListItem
    _ <- ws <* char ']'
    return $ List [content]

parseJsonListItem :: Parser Item
parseJsonListItem = do
    items <- many (parseJsonTextElem <|> parseJsonEnsemble
        <|> parseJsonParagraph)
    return $ Item items

parseJsonLink :: Parser Content
parseJsonLink = do
    _ <- string "\"link\"" *> ws *> char ':' *> ws *> char '{' *> ws
    url <- jsonPair "url" <* char ',' <* ws
    _ <- string "\"content\"" *> ws *> char ':'
    content <- parseJsonParagraph
    _ <- char '}' <* ws
    return $ Link (url, content)

parseJsonImage :: Parser Content
parseJsonImage = do
    _ <- string "\"image\"" *> ws *> char ':' *> ws *> char '{' *> ws
    url <- jsonPair "url" <* char ',' <* ws
    _ <- string "\"alt\"" *> ws *> char ':'
    content <- parseJsonParagraph
    _ <- char '}' <* ws
    return $ Image (url, content)

parseJsonContent :: Parser [Content]
parseJsonContent = do
    _ <- ws
    _ <- string "\"body\"" *> ws *> char ':' *> ws *> char '['
    content <- many (parseJsonParagraph <|> parseJsonEnsemble
        <|> parseJsonTextElem)
    _ <- ws *> char ']'
    return content

parseJsonObject :: Parser Document
parseJsonObject = do
    _ <- char '{' *> ws
    header <- parseHeader
    _ <- ws *> char ','
    content <- parseJsonContent
    _ <- ws *> char '}'
    return $ Document header content

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
    (x:xs) | p x -> Just (x, xs)
    _ -> Nothing
