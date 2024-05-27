{-
-- EPITECH PROJECT, 2024
-- mypandoc
-- File description:
-- ToMD
-}

module Output.ToMd
    (
        prettyPrintMDDocument
    ) where

import Types (Header(..), Content(..), Item(..), Document(..))
import Data.Char (isDigit)

prettyPrintMDDocument :: Document -> String
prettyPrintMDDocument (Document header contents) =
    "---\n" ++
    prettyPrintHeader header ++
    "---\n\n" ++
    concatMap (\(content, nextContent) -> case content of
        Paragraph _ -> prettyPrintContent content True nextContent
        _ -> prettyPrintContent content True nextContent) (zipWith (,) contents
        (drop 1 contents ++ [TextElement ""]))

prettyPrintHeader :: Header -> String
prettyPrintHeader (Header htitle hauthor hdate) =
    "title: " ++ htitle ++ "\n" ++
    maybe "" (\a -> "author: " ++ a ++ "\n") hauthor ++
    maybe "" (\d -> "date: " ++ d ++ "\n") hdate

prettyPrintSection :: (String, [Content]) -> String
prettyPrintSection ("", _) = ""
prettyPrintSection (stitle, _) = 
    let headerNumber = read (filter isDigit stitle) :: Int
        hashes = replicate headerNumber '#'
    in "\n" ++ hashes ++ " " ++ stitle ++ "\n\n"

prettyPrintLink :: (String, Content) -> String
prettyPrintLink (url, contents) = "[" ++ prettyPrintContent contents False
    (TextElement "")
    ++ "](" ++ url ++ ")"

prettyPrintParagraph :: Content -> Bool -> Content -> String
prettyPrintParagraph (Paragraph contents) True nextContent
    | any isLinkOrImage contents = concatMap (\(content, nextContent') ->
        prettyPrintContent
        content False nextContent') (zipWith (,) contents (drop 1 contents
        ++ [TextElement ""])) ++ "\n"
    | otherwise = concatMap (\(content, nextContent') -> prettyPrintContent
        content False nextContent')
        (zipWith (,) contents (drop 1 contents ++ [TextElement ""]))
        ++ if isParagraph nextContent && not (any isLinkOrImage contents)
            then "\n\n" else "\n"
prettyPrintParagraph (Paragraph contents) False _ = concatMap (
        \(content, nextContent') ->
        prettyPrintContent content False nextContent') (zipWith (,)
        contents (drop 1 contents ++ [TextElement ""]))
prettyPrintParagraph content _ _ = prettyPrintContent content False
    (TextElement "")

isLinkOrImage :: Content -> Bool
isLinkOrImage (Link _) = True
isLinkOrImage (Image _) = True
isLinkOrImage _ = False

isParagraph :: Content -> Bool
isParagraph (Paragraph _) = True
isParagraph _ = False

prettyPrintContent :: Content -> Bool -> Content -> String
prettyPrintContent (TextElement text) _ _ = text
prettyPrintContent (Italic text) _ _ = "*" ++ text ++ "*"
prettyPrintContent (Bold text) _ _ = "**" ++ text ++ "**"
prettyPrintContent (Code text) _ _ = "`" ++ text ++ "`"
prettyPrintContent (Link (url, contents)) _ _ = prettyPrintLink (url, contents)
prettyPrintContent (Image (url, contents)) _ _ = 
    "![" ++ prettyPrintContent contents False (TextElement "") ++ "]("
    ++ url ++ ")"
prettyPrintContent (Paragraph contents) jL nextContent = prettyPrintParagraph (
                    Paragraph contents) jL nextContent
prettyPrintContent (Section (ctitle, contents)) _ _ = prettyPrintSection (
                    ctitle, contents)
    ++ concatMap (\(content, nextContent) -> prettyPrintContent content True
    nextContent) (zipWith (,) contents (drop 1 contents ++ [TextElement ""]))
prettyPrintContent (CodeBlock contents) _ _ = 
    "```\n" ++ prettyPrintContent contents False (TextElement "") ++ "\n```\n"
prettyPrintContent (List items) _ _ = concatMap prettyPrintItem items ++ "\n"

prettyPrintItem :: Item -> String
prettyPrintItem (Item contents) = 
    concatMap (\(x, nextContent) -> "- " ++ prettyPrintContent x False
    nextContent ++ "\n")(zipWith (,) contents (drop 1 contents
    ++ [TextElement ""]))