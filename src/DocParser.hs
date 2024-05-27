{-
-- EPITECH PROJECT, 2024
-- compressor
-- File description:
-- Main
-}

module DocParser
    (
        readDocument,
    ) where

import ArgsParser (Options(..))

readDocument :: Options -> IO String
readDocument opts = readFile (optInput opts)