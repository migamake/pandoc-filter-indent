{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
-- | Common token representation used.
module Token(MyTok(..), MyLoc(..), Tokenized, line, col) where

import Data.Text(Text)
import Data.Tuple.Optics
import Optics.TH

-- * Common tokens and locations
-- Location is just line and column
data MyLoc = MyLoc { _line, _col :: Int }
  deriving (Eq, Ord, Show)

makeLenses ''MyLoc

-- | Token just classifies to blank, operator, and the style class
data MyTok =
    TBlank
  | TOperator
  | TKeyword
  | TCons
  | TVar
  | TNum
  | TOther
  deriving (Eq, Ord, Show)

-- | Records tokenized and converted to common token format.
type Tokenized = (MyTok, MyLoc, Text)

