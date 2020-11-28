{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Text.Pandoc.JSON
import           Text.Pandoc.Definition ()
import           Data.String (fromString, IsString)
import           Data.Text (Text)
import qualified Data.Text as T

import Filter(findColumns)

main :: IO ()
main = toJSONFilter blockFormatter

-- | Select the desired format output then process it.
blockFormatter :: Maybe Format -> Block -> Block
blockFormatter (Just (Format format)) (CodeBlock attrs content)
    | isHaskell attrs = codeFormatter format attrs content
    | otherwise = CodeBlock attrs content
-- Do not touch other blocks than 'CodeBlock'
blockFormatter _ x = x

-- | Select formatter
codeFormatter :: Text -> Attr -> Text -> Block
codeFormatter format attrs content | True =
        CodeBlock attrs $ T.pack $ show $ findColumns content
    {-
    | format == (fromString "latex") =
        RawBlock (Format "latex") (renderLatex content)
    | format == (fromString "html") =
        RawBlock (Format "html") (renderHtmlO content)-}
    -- Unknown formats gives the original elem.
    | otherwise = CodeBlock attrs content

--  (Text, [Text], [(Text, Text)])
isHaskell :: (Foldable t, Eq a1, IsString a1) =>
                   (a2, t a1, c) -> Bool
isHaskell (_, classes, _) = "haskell" `elem` classes
