{-
-- EPITECH PROJECT, 2024
-- compressor
-- File description:
-- Main
-}

module Lib
    (
        Options(..),
        usage,
        parseArgs,
        readDocument,
        allowedFormats,
        parseJsonObject,
        parseXmlObject,
        wsXML,
        ws,
        prettyPrintXMLDocument,
        prettyPrintJsonDocument,
        prettyPrint,
        insertTabs,
        verifyXml,
        runParser,
        prettyPrintMDDocument,
        Header(..),
        Content(..),
        Item(..),
        OutputType(..),
        Document(..)
    ) where

import ArgsParser (parseArgs, Options(..), allowedFormats)
import Errors (usage)
import DocParser (readDocument)
import Input.InJson (parseJsonObject, ws)
import Input.InXml (verifyXml, parseXmlObject, wsXML, verifyXml)
import Output.ToXml (prettyPrintXMLDocument)
import Output.ToJson (prettyPrintJsonDocument)
import Output.ToMd (prettyPrintMDDocument)
import Input.Parser (runParser)
import Output.Display (prettyPrint)
import Output.Common (insertTabs)
import Types (Header(..), Content(..), Item(..), OutputType(..), Document(..))
