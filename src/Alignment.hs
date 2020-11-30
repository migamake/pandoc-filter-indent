{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
module Alignment where

import Data.Text (Text)
import Data.Tuple.Optics
import Optics.Lens

import Token
import Tuples

-- | Datatype to present columns with alignment requirements.
data Align =
    ALeft
  | ACenter
  deriving (Eq, Ord, Show)

-- | Records tokenized and converted to common token format.
type Processed = (MyTok, MyLoc, Text, Maybe Int, Maybe (Align, Int))

-- | Access text content.
textContent :: Field3 a a Text Text => Lens' a Text
textContent  = _3


