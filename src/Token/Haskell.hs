{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns          #-}
-- | Haskell code tokenizer
module Token.Haskell(tokenizer) where

import Control.Arrow(first)
import Text.Pandoc.JSON
import Text.Pandoc.Definition ()
import Data.Function(on)
import Data.String (fromString, IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List(groupBy, sortBy)
import Debug.Trace(trace)
import Prelude hiding(getLine)

import GHC.SyntaxHighlighter

import Token

-- * Haskell tokenizer frontend
tokenizer :: Text -> Maybe [(MyTok, MyLoc, Text)]
tokenizer  = fmap ( splitTokens
                  . restoreLocations
                  . (first haskellTok <$>) )
           . tokenizeHaskell

haskellTok SymbolTok  = TOperator
haskellTok SpaceTok   = TBlank
haskellTok CommentTok = TBlank
haskellTok PragmaTok  = TBlank
haskellTok KeywordTok = TKeyword
haskellTok t          = TOther

locLine (Loc startLineNo startColNo _ _) = startLineNo
locCol  (Loc startLineNo startColNo _ _) = startColNo

-- | Split tokens into one blank per line.
splitTokens :: [(MyTok, MyLoc, Text)] -> [(MyTok, MyLoc, Text)]
splitTokens = mconcat
            . fmap splitter
  where
    splitter :: (MyTok, MyLoc, Text) -> [(MyTok, MyLoc, Text)]
    splitter (TBlank, loc@(MyLoc line _), txt) | T.filter (=='\n') txt /= "" =
        withLocs withNewLines
      where
        split, withNewLines :: [Text]
        split = T.lines txt
        withNewLines = fmap (<>"\n") (init split)
                    <> [last split]
        withLocs :: [Text] -> [(MyTok, MyLoc, Text)]
        withLocs (l:ls) = (TBlank, loc, l)
                        : zipWith mkEntry [line+1..] ls
        mkEntry :: Int -> Text -> (MyTok, MyLoc, Text)
        mkEntry i t = (TBlank, MyLoc i 1, t)
    splitter  other             = [other]

-- | Restore locations
restoreLocations :: [(a, Text)] -> [(a, MyLoc, Text)]
restoreLocations = go 1 1
  where
    go line col []              = []
    go line col ((tok, txt):ls) =
        (tok, MyLoc line col, txt):go newLine newCol ls
      where
        newLine  = line + lineIncr
        lineIncr = T.length (T.filter (=='\n') txt)
        newCol  | lineIncr == 0 = col + T.length txt
                | otherwise     = T.length
                                $ fst
                                $ T.break (=='\n')
                                $ T.reverse txt

