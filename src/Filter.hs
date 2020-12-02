{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
-- | Filtering a single code block.
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
import qualified Render.Latex
import qualified Render.HTML

import Debug.Trace(trace)

-- | Render a list of `Processed` token records into the target output format.
render ::  Format     -- ^ Format string
       ->  Attr       -- ^ Attributes
       -> [Processed] -- ^ Data about alignment
       ->  Block
--render "text" attrs aligned = RawBlock (Format "latex") $ processLatex aligned -- debug
render (Format "text" ) attrs = CodeBlock attrs           . Render.Debug.render
render (Format "latex") attrs = RawBlock (Format "latex") . processLatex
render (Format "html" ) attrs = RawBlock (Format "html" ) . processHTML
-- Debugging option
render other   attrs = CodeBlock attrs . T.pack . show

-- | Convert a list of input token records to raw LaTeX.
processLatex :: [Processed] -> T.Text
processLatex processed = Render.Latex.latexFromColSpans (length $ tableColumns processed)
                       $ colspans processed

-- | Convert a list of input token records to raw HTML.
processHTML :: [Processed] -> T.Text
processHTML  = Render.HTML.htmlFromColSpans
             . colspans
