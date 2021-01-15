{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
-- | Skylighting code tokenizer
module Token.Skylighting(lookupTokenizer, tokenizer) where

import Control.Arrow(first)
import Text.Pandoc.JSON ()
import Text.Pandoc.Definition ()
import Data.Maybe(listToMaybe, catMaybes)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding(getLine)
import Optics.Core

import qualified Skylighting.Types     as Sky
import           Skylighting.Types           (TokenType(..), Syntax, SourceLine, Token)
import qualified Skylighting.Syntax    as Sky(defaultSyntaxMap)
import qualified Skylighting.Tokenizer as Sky(tokenize, TokenizerConfig(..))
import qualified Skylighting.Core      as Sky(lookupSyntax, syntaxByShortName)

import Token ( MyLoc(MyLoc), MyTok(..), unTikzMark, mark )

rightToMaybe (Left  err   ) = Nothing
rightToMaybe (Right result) = Just result

lookupTokenizer :: [Text] -> Maybe Syntax
lookupTokenizer  = listToMaybe
                 . catMaybes
                 . fmap (Sky.syntaxByShortName Sky.defaultSyntaxMap)

-- * Haskell tokenizer frontend
-- | Attempt to tokenize input,
--   returns `Nothing` if unsuccessful,
--   so the processor can just pass input
--   further when tokenizer fails.
tokenizer :: Syntax -- Skylighting syntax description
          -> Text -- ^ Input text of code block
          -> Maybe [(MyTok, MyLoc, Text)]
tokenizer syntax =
    fmap ( joinEscapedOperators
         . splitTokens
         . restoreLocations
         . recognizeTokens )
    . rightToMaybe
    . Sky.tokenize tokenizerOpts syntax
  where
    tokenizerOpts = Sky.TokenizerConfig Sky.defaultSyntaxMap False

-- | Recognize tokens from all source lines.
recognizeTokens :: [SourceLine] -> [[(MyTok, Text)]]
recognizeTokens  = map $ map $ first skyTok

-- | Convert token type of `ghc-lib` into tokens recognized by the filter.
skyTok :: TokenType -> MyTok
skyTok FloatTok  = TNum
skyTok FloatTok  = TNum
skyTok DecValTok  = TNum
skyTok BaseNTok  = TNum
skyTok StringTok = TOther
skyTok VerbatimStringTok = TOther
skyTok SpecialStringTok = TOther
skyTok ConstantTok = TOther
skyTok KeywordTok = TKeyword
skyTok BuiltInTok = TKeyword
skyTok CharTok = TOther
skyTok SpecialCharTok = TOther
skyTok CommentTok = TBlank
skyTok DocumentationTok = TBlank
skyTok CommentTok = TOther
skyTok OperatorTok = TOperator
skyTok SpecialCharTok = TOperator
skyTok RegionMarkerTok = TOperator
skyTok AnnotationTok = TOther
skyTok ControlFlowTok = TKeyword
skyTok VariableTok = TVar
skyTok DataTypeTok = TCons
skyTok other     = TOther

-- FIXME: generalize for GHC tokenizer and Skylighting
-- | Restore locations
-- TESTME: test
-- 1. Without newlines should return a list of indices up to length
-- 2. Of the same length as number of tokens
-- 3. With newlines should return line indices up to number of lines.
-- 4. Same for a list of lists of words without newlines joined as lines
restoreLocations :: [[(MyTok, Text)]] -> [(MyTok, MyLoc, Text)]
restoreLocations srcLines = concat
                          $ zipWith (`go` 1) [1..] srcLines
  where
    go line col []              = []
    go line col ((tok, txt):ls) =
        (tok, MyLoc line col (isMark tok), txt):go newLine newCol ls
      where
        isMark TBlank = False
        isMark _      = True
        newLine  = line + lineIncr
        lineIncr = T.length $ T.filter (=='\n') txt
        newCol  | lineIncr == 0 = col + T.length txt
                | otherwise     = (+1)
                                $ T.length
                                $ fst
                                $ T.break (=='\n')
                                $ T.reverse txt

-- * Likely common with other tokenizers
-- | Split tokens into one blank per line.
-- TESTME: assures that no token has '\n' before the end of text.
splitTokens :: [(MyTok, MyLoc, Text)] -> [(MyTok, MyLoc, Text)]
splitTokens = mconcat
            . fmap splitter
  where
    splitter :: (MyTok, MyLoc, Text) -> [(MyTok, MyLoc, Text)]
    splitter (TBlank, loc@(MyLoc line _ _), txt) | T.filter (=='\n') txt /= "" =
        withLocs withNewLines
      where
        split, withNewLines :: [Text]
        split = T.lines txt
        withNewLines = fmap (<>"\n") (init split)
                    <> [last split]
        withLocs :: [Text] -> [(MyTok, MyLoc, Text)]
        withLocs (l:ls) = (TBlank, set mark True $ loc, l)
                        : zipWith mkEntry [line+1..] ls
        mkEntry :: Int -> Text -> (MyTok, MyLoc, Text)
        mkEntry i t = (TBlank, MyLoc i 1 True, t)
    splitter other@(_, loc@(MyLoc line 1 x), txt) = [set (_2 % mark) True other]
    splitter other                                = [other]


unmark :: Field2 a a MyLoc MyLoc => a -> a
unmark = set (_2 % mark) False

-- FIXME: use no-indent-mark instead.
joinEscapedOperators :: (Eq c, IsString c, Semigroup c) => [(MyTok, MyLoc, c)] -> [(MyTok, MyLoc, c)]
joinEscapedOperators (a@(_, _, "("):b@(_, _, _):c@(_, _, ")"):rest) =
   a:unmark b:unmark c:joinEscapedOperators rest
joinEscapedOperators (a@(_,    loc, "`"):b@(_, _, _):c@(_, _, "`"):rest) =
   a:unmark b:unmark c:joinEscapedOperators rest
joinEscapedOperators (a@(_, _, "("):b@(TOperator, _, _):rest) =
   a:unmark b:joinEscapedOperators rest
joinEscapedOperators (a@(_, _, _):b@(_, _, ")"):rest) =
   a:unmark b:joinEscapedOperators rest
joinEscapedOperators (tok:rest) = tok:joinEscapedOperators rest
joinEscapedOperators []         = []

{-

-- | Recognize token using both token type from `ghc-lib`,
--   and text content.
--   Only TikZ marks are recognized by looking up text content.
recognizeToken :: [Token] -> (MyTok, Text)
recognizeToken (CommentTok, tokText@(unTikzMark -> Just mark)) =
  (TTikz mark,           tokText)
recognizeToken (tokType, tokText) =
  (skyTok       tokType, tokText)
 -}

