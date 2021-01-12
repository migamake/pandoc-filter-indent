{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
-- | Filtering a single code block.
module Filter(renderBlock, renderInline) where

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
renderBlock ::  Format     -- ^ Format string
            ->  Attr       -- ^ Attributes
            -> [Processed] -- ^ Data about alignment
            ->  Block
--render "text" attrs aligned = RawBlock (Format "latex") $ processLatex aligned -- debug
renderBlock (Format "text" ) attrs = CodeBlock attrs           . Render.Debug.render
renderBlock (Format "latex") attrs = RawBlock (Format "latex") . processLatex
renderBlock (Format "html" ) attrs = RawBlock (Format "html" ) . processHTML
-- Debugging option
renderBlock other            attrs = CodeBlock attrs           . T.pack . show

-- TODO: inline should strip colspans, and ignore table
-- | Render a list of `Processed` token records into the target output format.
renderInline ::  Format     -- ^ Format string
             ->  Attr       -- ^ Attributes
             -> [Processed] -- ^ Data about alignment
             ->  Inline
--render "text" attrs aligned = RawBlock (Format "latex") $ processLatex aligned -- debug
renderInline (Format "text" ) attrs = Code      attrs            . Render.Debug.render
renderInline (Format "latex") attrs = RawInline (Format "latex") . processLatex
renderInline (Format "html" ) attrs = RawInline (Format "html" ) . processHTML
-- Debugging option
renderInline other            attrs = Code      attrs            . T.pack . show

-- | Convert a list of input token records to raw LaTeX.
processLatex :: [Processed] -> T.Text
processLatex processed = Render.Latex.latexFromColSpans (length $ tableColumns processed)
                       $ colspans processed

-- | Convert a list of input token records to raw HTML.
processHTML :: [Processed] -> T.Text
processHTML  = Render.HTML.htmlFromColSpans
             . colspans
