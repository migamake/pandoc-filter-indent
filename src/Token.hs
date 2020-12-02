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
  | TTikz  Text -- TikZmark in a comment
  deriving (Eq, Ord, Show)

-- | Records tokenized and converted to common token format.
type Tokenized = (MyTok, MyLoc, Text)

unTikzMark    :: Text -> Maybe Text
unTikzMark txt =
  unwrap "{->" "-}" txt >>= \case
    ""   -> Nothing
    mark -> Just mark

unwrap :: Text -> Text -> Text -> Maybe Text
unwrap starter trailer   txt  |
  starter `T.isPrefixOf` txt &&
  trailer `T.isPrefixOf` txt  =
      Just
    $ T.dropEnd (T.length trailer)
    $ T.drop    (T.length starter) txt
unwrap _       _         _    = Nothing

