{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
module Filter where

import Text.Pandoc.JSON
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding(getLine)

import Token.Haskell
import FindColumns
import Alignment
import Render.ColSpan
import qualified Render.Debug(render)

filterCodeBlock = withTokens findColumns ("haskell", tokenizer)

-- | Apply function if tokenization succeeded, otherwise return same output
withTokens :: Show a => (t -> p) -> (String, a -> Maybe t) -> a -> p
withTokens f (tokenizerName, tokenizer) src@(tokenizer -> Nothing) = error $ mconcat ["Tokenizer ", tokenizerName, " failed for ", show src]
withTokens f (tokenizerName, tokenizer)     (tokenizer -> Just tokens) = f tokens

render ::  Text       -- ^ Format string
       ->  Attr       -- ^ Attributes
       -> [Processed] -- ^ Data about alignment
       ->  Block
render "text" attrs aligned = CodeBlock attrs $ Render.Debug.render aligned
render other  attrs aligned = CodeBlock attrs $ T.pack $ show aligned

