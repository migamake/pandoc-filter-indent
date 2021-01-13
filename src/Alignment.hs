{-# LANGUAGE FlexibleContexts #-}
-- | Alignment options,
--   and extracting common columns
--   from tuples used in analysis.
module Alignment where

import Data.Text (Text)
import Data.Tuple.Optics
import Optics.Lens

import Token ( MyLoc, MyTok )

-- | Datatype to present columns with alignment requirements.
data Align =
    ALeft
  | ACenter
  | AIndent -- indentation spacing
  deriving (Eq, Ord, Show)

-- | Records tokenized and converted to common token format.
type Processed = (MyTok -- ^ Token type
                 ,MyLoc -- ^ Token location
                 ,Text  -- ^ Text content
                 ,Maybe Int -- ^ Indent column
                 ,Maybe (Align -- ^ Alignment mark
                        ,Int)  -- ^ Alignment column
                 )

 

-- | Access text content.
tokenType :: Field1 a a MyTok MyTok => Lens' a MyTok
tokenType  = _1

-- | Access text content.
textContent :: Field3 a a Text Text => Lens' a Text
textContent  = _3


