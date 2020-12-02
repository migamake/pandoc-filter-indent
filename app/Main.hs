{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Main where

import           Text.Pandoc.JSON
import           Text.Pandoc.Definition ()
import           Data.String (fromString, IsString)
import           Data.Maybe  (fromMaybe)
import           Data.Text   (Text)
import qualified Data.Text as T

import Debug.Trace(trace)

import Token.Haskell
import GHC.SyntaxHighlighter
import Filter
import FindColumns

main :: IO ()
main = toJSONFilter blockFormatter

-- | Select the desired format output then process it.
blockFormatter :: Maybe Format -> Block -> Block
blockFormatter format (CodeBlock attrs content) =
    codeFormatter (fromMaybe (Format "text") format) attrs content
-- Do not touch other blocks than 'CodeBlock'
blockFormatter _       x                        = x

-- | Run tokenizer, analysis, and formatter.
--   Fallback to original input, if failed to tokenize.
codeFormatter :: Format -- ^ Output format, defaults to "text" if not found
              -> Attr -- ^ Code block attributes
              -> Text -- ^ Code block content
              -> Block
codeFormatter format attrs content =
  case fmap findColumns $ aTokenizer content of
    Just processed -> render format attrs processed
    Nothing        -> CodeBlock attrs content -- fallback
  where
    aTokenizer | isHaskell attrs = Token.Haskell.tokenizer
               | otherwise       = \_ -> Nothing

-- | Check if the code block is tagged as Haskell.
isHaskell :: (Foldable t, Eq a1, IsString a1) =>
                   (a2, t a1, c) -> Bool
isHaskell (_, classes, _) = "haskell" `elem` classes
