{-
-- EPITECH PROJECT, 2024
-- compressor
-- File description:
-- Main
-}

module Errors (usage) where

import System.Exit

-- Usage information
-- usage :: String
-- usage = "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]\n" ++
--     "• -i : path to input file (mandatory)\n" ++
--     "• -f : format of the output (mandatory)\n" ++
--     "• -o : path to the output file\n" ++
--     "• -e : format of the input file"
--same but exit with error code 84
usage :: IO ()
usage =
    putStrLn "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]" >>
    putStrLn "• -i : path to input file (mandatory)" >>
    putStrLn "• -f : format of the output (mandatory)" >>
    putStrLn "• -o : path to the output file" >>
    putStrLn "• -e : format of the input file" >>
    exitWith (ExitFailure 84)
