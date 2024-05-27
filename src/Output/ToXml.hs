{-
-- EPITECH PROJECT, 2024
-- mypandoc
-- File description:
-- ToXml
-}

module Output.ToXml
    (
        prettyPrintXMLDocument
    ) where

import Output.Common (insertTabs)
import Types (Header(..), Content(..), Item(..), Document(..))

prettyPrintXMLDocument :: Document -> String
prettyPrintXMLDocument (Document header contents) =
    "<document>\n" ++
    prettyPrintHeader header 1 ++
    insertTabs 1 ++ "<body>\n" ++
    concatMap (\content -> prettyPrintContent content 2 False) contents ++
    insertTabs 1 ++ "</body>\n" ++
    "</document>\n"

prettyPrintHeader :: Header -> Int -> String
prettyPrintHeader (Header htitle hauthor hdate) level =
    insertTabs level ++ "<header title=\"" ++ htitle ++ "\">\n" ++
    maybe "" (\a -> insertTabs (level + 1) ++ "<author>" ++ a
        ++ "</author>\n") hauthor ++
    maybe "" (\d -> insertTabs (level + 1) ++ "<date>" ++ d
        ++ "</date>\n") hdate ++
    insertTabs level ++ "</header>\n"

prettyPrintLink :: (String, Content) -> String
prettyPrintLink (url, contents) = "<link url=\"" ++ url ++ "\">"
    ++ concatMap (\content -> case content of
        Paragraph _ -> prettyPrintContent content 0 True
        _ -> prettyPrintContent content 0 False) [contents]
    ++ "</link>"

prettyPrintImage :: (String, Content) -> String
prettyPrintImage (url, contents) = "<image url=\"" ++ url ++ "\">"
    ++ concatMap (\content -> case content of
        Paragraph _ -> prettyPrintContent content 0 True
        _ -> prettyPrintContent content 0 False) [contents]
    ++ "</image>"

prettyPrintParagraph :: Content -> Int -> Bool -> String
prettyPrintParagraph (Paragraph contents) level True = insertTabs level
    ++ concatMap (\content -> prettyPrintContent content (level + 1) False)
        contents
prettyPrintParagraph (Paragraph contents) level False = insertTabs level
    ++ "<paragraph>"
    ++ concatMap (\content -> prettyPrintContent content (level + 1) False)
        contents
    ++ "</paragraph>\n"
prettyPrintParagraph content level _ = insertTabs level
    ++ "<paragraph>"
    ++ prettyPrintContent content (level + 1) False
    ++ "</paragraph>\n"

prettyPrintContent :: Content -> Int -> Bool -> String
prettyPrintContent (TextElement text) _ _ = text
prettyPrintContent (Italic text) _ _ = "<italic>" ++ text ++ "</italic>"
prettyPrintContent (Bold text) _ _ = "<bold>" ++ text ++ "</bold>"
prettyPrintContent (Code text) _ _ = "<code>" ++ text ++ "</code>"
prettyPrintContent (Link (url, contents)) _ _ = prettyPrintLink (url,
                    contents)
prettyPrintContent (Image (url, contents)) _ _ = prettyPrintImage (url,
                    contents)
prettyPrintContent (Paragraph contents) level isP = prettyPrintParagraph (
                    Paragraph contents) level isP
prettyPrintContent (Section (ctitle, contents)) level _ = insertTabs level
    ++ "<section title=\"" ++ ctitle ++ "\">\n"
    ++ concatMap (\content -> prettyPrintContent content (level + 1) False)
        contents
    ++ insertTabs level ++ "</section>\n"
prettyPrintContent (CodeBlock contents) level _ = insertTabs level
    ++ "<codeblock>\n"
    ++ prettyPrintContent contents (level + 1) False
    ++ insertTabs level ++ "</codeblock>\n"
prettyPrintContent (List items) level _ = insertTabs level
    ++ "<list>\n"
    ++ concatMap (\item -> prettyPrintItem item (level + 1)) items
    ++ insertTabs level ++ "</list>\n"

prettyPrintItem :: Item -> Int -> String
prettyPrintItem (Item contents) level =
    concatMap (\content -> prettyPrintContent content level False) contents
