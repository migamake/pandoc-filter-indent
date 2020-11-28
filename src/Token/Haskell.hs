{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns          #-}
-- | Haskell code tokenizer
module Token.Haskell(tokenizer) where

import Text.Pandoc.JSON
import Text.Pandoc.Definition ()
import Data.Function(on)
import Data.String (fromString, IsString)
import Data.Text (Text)
import Data.List(groupBy, sortBy)
import Debug.Trace(trace)
import Prelude hiding(getLine)

import GHC.SyntaxHighlighter

import Token

-- * Haskell tokenizer frontend
tokenizer :: Text -> Maybe [(MyTok, MyLoc, Text)]
tokenizer txt = fmap (fmap rehash) . zip <$> tokenizeHaskell    txt
                                         <*> tokenizeHaskellLoc txt 
  where
    rehash :: ((Token, Text), (Token, Loc)) -> (MyTok, MyLoc, Text)
    rehash ((tok, tokText), (tok_, loc)) = (haskellTok tok, MyLoc (locLine loc) (locCol loc), tokText)

haskellTok SymbolTok  = TOperator
haskellTok SpaceTok   = TBlank
haskellTok CommentTok = TBlank
haskellTok PragmaTok  = TBlank
haskellTok KeywordTok = TKeyword
haskellTok t          = TOther

locLine (Loc startLineNo startColNo _ _) = startLineNo
locCol  (Loc startLineNo startColNo _ _) = startColNo

