{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NamedFieldPuns    #-}
module Main where

import           Text.Pandoc.JSON
import           Text.Pandoc.Walk       (walk)
import           Text.Pandoc.Definition ()
import           Data.String            (IsString)
import           Data.Maybe             (fromMaybe)
import qualified Data.Map  as Map
import           Data.Text(Text)
import qualified Data.Text as T
              
import           Token.Haskell          (tokenizer)
import           Filter                 (renderBlock, renderInline)
import           FindColumns            (findColumns)
import           Alignment              (Processed)

data Options = Options {
    inlineSyntax :: Text
  }

main :: IO ()
main = toJSONFilter runner

runner :: Maybe Format -> Pandoc -> Pandoc
runner (fromMaybe (Format "text") -> format) input@(Pandoc (Meta meta) _) =
    walk (blockFormatter opts format) input
  where
    opts = case Map.lookup "inline-code" meta of
      Nothing                    -> Options "haskell" -- default
      Just (MetaString       s)  -> Options s -- never needed?
      Just (MetaInlines [Str s]) -> Options s
      Just  otherValue           -> error $ "inline-code: meta should be a string but is: " <> show otherValue

-- | Select the desired format output then process it.
--   Run tokenizer, analysis, and formatter.
--   Fallback to original input, if failed to tokenize.
blockFormatter :: Options
               -> Format -- ^ Output format, defaults to "plain" if not found
               -> Block
               -> Block
blockFormatter _opts format (CodeBlock attrs content) =
  case fmap findColumns $ getTokenizer attrs content of
    Just processed -> renderBlock format attrs processed
    Nothing        -> CodeBlock          attrs content -- fallback
-- Do not touch other blocks than 'CodeBock'
blockFormatter opts  format x                         =
    walk (inlineFormatter opts format) x

-- | Select the desired format output then process it.
--   Run tokenizer, and inline formatter.
--   Fallback to original input, if failed to tokenize.
inlineFormatter :: Options
                -> Format -- ^ Output format, defaults to "plain" if not found
                -> Inline
                -> Inline
inlineFormatter  Options {inlineSyntax} format (Code    attrs txt    ) =
    case getTokenizer attrs txt of
      Just processed -> renderInline format attrs $ map discardLoc processed
      Nothing        -> Code                attrs txt -- fallback
  where
    discardLoc (a, _, b) = (a, b)
    addClass (a, classes, b) | inlineSyntax /= "" = (a, inlineSyntax:classes, b)
    addClass  o                                   = o
inlineFormatter _opts _       x                       = x

getTokenizer attrs | isHaskell attrs = Token.Haskell.tokenizer
                   | otherwise       = \_ -> Nothing

-- | Check if the code block is tagged as Haskell.
isHaskell :: (Foldable t, Eq a1, IsString a1) =>
                   (a2, t a1, c) -> Bool
isHaskell (_, classes, _) = "haskell" `elem` classes
