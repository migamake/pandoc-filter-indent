{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NamedFieldPuns    #-}
module Main where

import           Text.Pandoc.JSON
import           Text.Pandoc.Walk       (walk)
import           Text.Pandoc.Definition ()
import           Data.String            (fromString, IsString)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text as T

import           Debug.Trace            (trace)
              
import           Token.Haskell          (tokenizer)
import           Filter                 (renderBlock, renderInline)
import           FindColumns            (findColumns)
import           Alignment              (Processed)
import           Opts

main :: IO ()
main = do
  options <- getOptions
  toJSONFilter $ blockFormatter options . fromMaybe (Format "text")

-- | Select the desired format output then process it.
blockFormatter :: Options -> Format -> Block -> Block
blockFormatter _opts format (CodeBlock attrs content) =
    codeFormatter blockRenderer format attrs content
-- Do not touch other blocks than 'CodeBock'
blockFormatter opts  format x                         =
    walk (inlineFormatter opts format) x

inlineFormatter :: Options -> Format -> Inline -> Inline
inlineFormatter  Options {inlineSyntax} format (Code    attrs txt    ) =
    codeFormatter inlineRenderer format (addClass attrs) txt
  where
      addClass (a, classes, b) | inlineSyntax /= "" = (a, inlineSyntax:classes, b)
      addClass  o                                   = o
inlineFormatter _opts _       x                       = x

-- | Renderer for the correctly processed fragments.
data Renderer a = Renderer {
    renderSuccess  :: Format -> Attr -> [Processed] -> a
  , renderFallback ::           Attr ->  Text       -> a
  }

-- | Rendering inline elements
inlineRenderer :: Renderer Inline
inlineRenderer = Renderer renderInline Code

-- | Rendering block elements
blockRenderer :: Renderer Block
blockRenderer  = Renderer renderBlock  CodeBlock

-- | Run tokenizer, analysis, and formatter.
--   Fallback to original input, if failed to tokenize.
codeFormatter :: Renderer a -- ^ Renderer to intended type
              -> Format     -- ^ Output format, defaults to "text" if not found
              -> Attr       -- ^ Code block attributes
              -> Text       -- ^ Code block content
              ->          a
codeFormatter Renderer {..} format attrs content =
  case fmap findColumns $ getTokenizer attrs content of
    Just processed -> renderSuccess format attrs processed
    Nothing        -> renderFallback       attrs content -- fallback

getTokenizer attrs | isHaskell attrs = Token.Haskell.tokenizer
                   | otherwise       = \_ -> Nothing

-- | Check if the code block is tagged as Haskell.
isHaskell :: (Foldable t, Eq a1, IsString a1) =>
                   (a2, t a1, c) -> Bool
isHaskell (_, classes, _) = "haskell" `elem` classes
