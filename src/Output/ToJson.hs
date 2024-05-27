{-
-- EPITECH PROJECT, 2024
-- mypandoc
-- File description:
-- ToJson
-}

module Output.ToJson
    (
        prettyPrintJsonDocument
    ) where

import Data.List (intercalate)
import Output.Common (insertTabs)
import Types (Header(..), Content(..), Item(..), Document(..))

prettyPrintJsonDocument :: Document -> String
prettyPrintJsonDocument (Document header contents) =
    "{\n" ++
    prettyPrintHeader header 1 ++
    insertTabs 1 ++ "\"body\": [\n" ++
    intercalate ",\n" (map (\content -> prettyPrintContent content 2 False)
        contents)
    ++ "\n" ++ insertTabs 1 ++ "]\n" ++ "}"

prettyPrintHeader :: Header -> Int -> String
prettyPrintHeader (Header htitle hauthor hdate) level =
    insertTabs level ++ "\"header\": {\n" ++
    insertTabs (level + 1) ++ "\"title\": \"" ++ htitle ++ "\",\n" ++
    maybe "" (\a -> insertTabs (level + 1) ++ "\"author\": \"" ++ a
        ++ "\",\n") hauthor ++
    maybe "" (\d -> insertTabs (level + 1) ++ "\"date\": \"" ++ d
        ++ "\"\n") hdate ++
    insertTabs level ++ "},\n"

prettyPrintParagraph :: [Content] -> Int -> Bool -> String
prettyPrintParagraph contents level displayWithoutBrackets =
    if displayWithoutBrackets
        then intercalate ",\n" (map (\content -> prettyPrintContent content
            (level) False) contents)
        else insertTabs level ++ "[\n" ++
            intercalate ",\n" (map (\content -> prettyPrintContent content
            (level + 1) False) contents) ++
            "\n" ++ insertTabs level ++ "]"

encB :: String -> Int -> (String, Bool)
encB content level
    | head content == '{' && last content == '}' = (content, False)
    | otherwise = (insertTabs level ++ "{\n" ++ insertTabs 1
    ++ content ++ "\n" ++ insertTabs level ++ "}", True)

prettyPrintSection :: (String, [Content]) -> Int -> String
prettyPrintSection (ctitle, contents) level = 
    let sectionContent = 
            insertTabs level ++ "\"section\": {\n" ++
            insertTabs (level + 2) ++ "\"title\": \"" ++ ctitle ++ "\",\n" ++
            insertTabs (level + 2) ++ "\"content\": [\n" ++ intercalate ",\n"
            (map (\content -> prettyPrintContent content (level + 3) False)
            contents) ++ "\n" ++ insertTabs (level + 2) ++ "]\n" ++
            insertTabs (level + 1) ++ "}"
        enclosedSectionContent = fst (encB sectionContent level)
    in enclosedSectionContent

lnkContentHelper :: Content -> Int -> String
lnkContentHelper (Paragraph cts) level = prettyPrintParagraph cts level True
lnkContentHelper content level = prettyPrintContent content level False

prettyPrintLink :: (String, Content) -> Int -> String
prettyPrintLink (url, contents) level = 
    let linkContent = 
            insertTabs level ++ "\"link\": {\n" ++
            insertTabs (level + 2) ++ "\"url\": \"" ++ url ++ "\",\n" ++
            insertTabs (level + 2) ++ "\"content\": [\n" ++
            lnkContentHelper contents (level + 3) ++
            "\n" ++ insertTabs (level + 2) ++ "]\n" ++
            insertTabs (level + 1) ++ "}"
        enclosedLinkContent = fst (encB linkContent level)
    in enclosedLinkContent

imgContentHelper :: Content -> Int -> String
imgContentHelper (Paragraph cts) level = prettyPrintParagraph cts level True
imgContentHelper content level = prettyPrintContent content level False

prettyPrintImage :: (String, Content) -> Int -> String
prettyPrintImage (url, contents) level = 
    let imageContent = 
            insertTabs level ++ "\"image\": {\n" ++
            insertTabs (level + 2) ++ "\"url\": \"" ++ url ++ "\",\n" ++
            insertTabs (level + 2) ++ "\"alt\": [\n" ++
            imgContentHelper contents (level + 3) ++
            "\n" ++ insertTabs (level + 2) ++ "]\n" ++
            insertTabs (level + 1) ++ "}"
        enclosedImageContent = fst (encB imageContent level)
    in enclosedImageContent

prettyPrintContent :: Content -> Int -> Bool -> String
prettyPrintContent (TextElement text) level _ = insertTabs level
    ++ "\"" ++ text ++ "\""
prettyPrintContent (Italic text) level _ = fst (encB (insertTabs level
    ++ "\"italic\": \"" ++ text ++ "\"") level)
prettyPrintContent (Bold text) level _ = fst (encB (insertTabs level
    ++ "\"bold\": \"" ++ text ++ "\"") level)
prettyPrintContent (Code text) level _ = fst (encB (insertTabs level
    ++ "\"code\": \"" ++ text ++ "\"") level)
prettyPrintContent (Link (url, contents)) level _ = prettyPrintLink (
    url, contents) level
prettyPrintContent (Image (url, contents)) level _ = prettyPrintImage (
    url, contents) level
prettyPrintContent (Paragraph cts) level isP = prettyPrintParagraph cts level
    isP
prettyPrintContent (Section (ctitle, contents)) level _ = prettyPrintSection (
    ctitle, contents) level
prettyPrintContent (CodeBlock contents) level _ =
    fst (encB (
        insertTabs level ++ "\"codeblock\": [\n" ++
        intercalate ",\n" (map (\content -> prettyPrintContent content
        (level + 2) False) [contents]) ++
        "\n" ++ insertTabs (level + 1) ++ "]"
    ) level)
prettyPrintContent (List items) level _ =
    fst (encB (
        insertTabs level ++ "\"list\": [\n" ++
        intercalate ",\n" (map (\item -> prettyPrintItem item
        (level + 2)) items) ++
        "\n" ++ insertTabs (level + 1) ++ "]"
    ) level)

prettyPrintItem :: Item -> Int -> String
prettyPrintItem (Item contents) level = 
    intercalate ",\n" (map (\content -> prettyPrintContent content
    (level) False) contents)
