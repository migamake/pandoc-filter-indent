{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
module Alignment where

import Data.Text (Text)

import Token
import Tuples

-- | Datatype to present columns with alignment requirements.
data Align =
    ALeft
  | ACenter
  | ANone -- no alignment, but this is a column where alignment happens for other lines
  deriving (Eq, Ord, Show)

-- | Records tokenized and converted to common token format.
type Aligned    = (MyTok, MyLoc, Text, Maybe Int, Maybe Align)

