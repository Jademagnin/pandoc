{-
-- EPITECH PROJECT, 2024
-- compressor
-- File description:
-- Main
-}

module Types (Header(..), Content(..), Item(..), Document(..),
  OutputType(..)) where

data Header = Header
  { title :: String
  , author :: Maybe String
  , date :: Maybe String
  } deriving (Show)

-- old:
-- data Content = TextElement String
--              | Italic String
--              | Bold String
--              | Code String
--              | Link (String, String)
--              | Image (String, String)
--              | Paragraph [Content]
--              | Section (String, [Content])
--              | CodeBlock String
--              | List [Item]
--              deriving (Show)

data Content = TextElement String
             | Italic String
             | Bold String
             | Code String
             | Link (String, Content)
             | Image (String, Content)
             | Paragraph [Content]
             | Section (String, [Content])
             | CodeBlock Content
             | List [Item]
             deriving (Show)

-- Item in a List
data Item = Item [Content] deriving (Show)
data Document = Document Header [Content] deriving (Show)
data OutputType = Json | Xml | Markdown deriving (Show, Enum, Bounded)
