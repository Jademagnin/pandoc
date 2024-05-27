{-
-- EPITECH PROJECT, 2024
-- mypandoc
-- File description:
-- Common
-}

module Output.Common
    (
        insertTabs
    ) where

insertTabs :: Int -> String
insertTabs n = replicate (n * 4) ' '
