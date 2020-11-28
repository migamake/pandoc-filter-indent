{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns          #-}
-- | Common token representation used.
module Token(MyTok(..), MyLoc(..), Tokenized) where

import Data.Text(Text)

-- * Common tokens and locations
-- Location is just line and column
data MyLoc = MyLoc { line, col :: Int }
  deriving (Eq, Ord, Show)

-- | Token just classifies to blank, operator, and the style class
data MyTok =
    TBlank
  | TOperator
  | TKeyword
  | TOther
  deriving (Eq, Ord, Show)

-- | Records tokenized and converted to common token format.
type Tokenized = (MyTok, MyLoc, Text)

