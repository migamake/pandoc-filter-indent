{-# LANGUAGE FlexibleContexts #-}
-- | Alignment options,
--   and extracting common columns
--   from tuples used in analysis.
module Alignment where

import Data.Text         ( Text )
import Data.Tuple.Optics ( Field1(..), Field3(..) )
import Optics.Lens       ( Lens' )

import Token ( MyLoc, MyTok )

-- | Datatype to present columns with alignment requirements.
data Align =
    ALeft   -- ^ Align to the left of the cell
  | ACenter -- ^ Align to the center of the cell
  | AIndent -- ^ Indentation spacing: whitespace with minimum width to preserve
  deriving (Eq, Ord, Show)

-- | Records tokenized and converted to common token format.
type Processed = (MyTok        -- Token type
                 ,MyLoc        -- Token location
                 ,Text         -- Text content
                 ,Maybe Int    -- Indent column
                 ,Maybe (Align -- Alignment mark
                        ,Int)  -- Alignment column
                 )

 

-- | Access text content.
tokenType :: Field1 a a MyTok MyTok => Lens' a MyTok
tokenType  = _1

-- | Access text content.
textContent :: Field3 a a Text Text => Lens' a Text
textContent  = _3


