{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
-- | Common token representation used.
module Token(MyTok(..), MyLoc(..), Tokenized, line, col, unwrap, unTikzMark) where

import           Data.Maybe(fromMaybe)
import           Data.Text(Text)
import qualified Data.Text as T
import           Data.Tuple.Optics
import           Optics.TH

-- * Common tokens and locations
--   We keep them here, so we can translate output from tokenizers to common format.
-- | Location is just line and column (not a slice.)
data MyLoc = MyLoc { _line :: Int -- ^ Line number starting from 1
                   , _col  :: Int -- ^ Column number starting from 1
                   }
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
  | TTikz  Text -- TikZmark in a comment
  | TMulti -- multi token
  deriving (Eq, Ord, Show)

-- | Records tokenized and converted to common token format.
type Tokenized = (MyTok, MyLoc, Text)

-- | Unpack a Haskell comment with a TikZ mark indicator.
unTikzMark    :: Text -> Maybe Text
unTikzMark txt =
  unwrap "{->" "-}" txt >>= \case
    ""   -> Nothing
    mark -> Just mark

-- | Given opening text, and closing text,
--   check that input is "braced" by these, and strip them.
--   Return `Nothing` if input text does not match.
unwrap :: Text -- ^ Opening text
       -> Text -- ^ Closing text
       -> Text -- ^ Input to match
       -> Maybe Text
unwrap starter trailer   txt  |
  starter `T.isPrefixOf` txt &&
  trailer `T.isPrefixOf` txt  =
      Just
    $ T.dropEnd (T.length trailer)
    $ T.drop    (T.length starter) txt
unwrap _       _         _    = Nothing

