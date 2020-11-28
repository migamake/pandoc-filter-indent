{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Filter where

import Text.Pandoc.JSON
import Text.Pandoc.Definition ()
import Data.String (fromString, IsString)
import Data.Text (Text)

import GHC.SyntaxHighlighter


tokenizer txt = zip <$> tokenizeHaskell    txt
                    <*> tokenizeHaskellLoc txt 

findColumns :: Text -> _
findColumns = tokenizer

