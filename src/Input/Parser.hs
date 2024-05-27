{--
-- EPITECH PROJECT, 2024
-- B-FUN-400-STG-4-1-mypandoc-jade.magnin
-- File description:
-- Parser
--}

module Input.Parser
    (
        Parser(..)
    ) where

import Control.Applicative (Alternative(..))

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap f parser = Parser $ \input -> 
        case runParser parser input of
            Just (result, rest) -> Just (f result, rest)
            Nothing -> Nothing

instance Applicative Parser where
    pure a = Parser $ \input -> Just (a, input)
    p1 <*> p2 = Parser $ \input -> 
        case runParser p1 input of
            Just (f, rest) -> runParser (fmap f p2) rest
            Nothing -> Nothing

instance Alternative Parser where
    empty = Parser $ const Nothing
    p1 <|> p2 = Parser $ \xs ->
        case runParser p1 xs of
            Nothing -> runParser p2 xs
            Just result -> Just result

instance Monad Parser where
    p >>= f = Parser $ \input -> 
        case runParser p input of
            Just (result, rest) -> runParser (f result) rest
            Nothing -> Nothing
