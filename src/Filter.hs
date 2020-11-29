{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
module Filter where

import Text.Pandoc.JSON
import Text.Pandoc.Definition ()
import Data.Function(on)
import Data.String (fromString, IsString)
import Data.Text (Text)
import Data.List(groupBy, sortBy, sort, group)
import Prelude hiding(getLine)
import Optics.Core
import Data.Tuple.Optics

import Token
import Token.Haskell
import Tuples
import FindColumns
import Alignment

filterCodeBlock = withTokens findColumns ("haskell", tokenizer)

-- | Apply function if tokenization succeeded, otherwise return same output
withTokens f (tokenizerName, tokenizer) src@(tokenizer -> Nothing) = error $ mconcat ["Tokenizer ", tokenizerName, " failed for ", show src]
withTokens f (tokenizerName, tokenizer)     (tokenizer -> Just tokens) = f tokens

