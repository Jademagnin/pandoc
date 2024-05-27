{-
-- EPITECH PROJECT, 2024
-- compressor
-- File description:
-- Main
-}

module Output.Display
    (
        prettyPrint
    ) where

import Types (Document(..), OutputType(..))
import Output.ToXml (prettyPrintXMLDocument)
import Output.ToJson (prettyPrintJsonDocument)
import Output.ToMd (prettyPrintMDDocument)

prettyPrint :: Document -> OutputType -> String
prettyPrint (Document header contents) docType =
    case docType of
        Xml -> prettyPrintXMLDocument (Document header contents)
        Json -> prettyPrintJsonDocument (Document header contents)
        Markdown -> prettyPrintMDDocument (Document header contents)
