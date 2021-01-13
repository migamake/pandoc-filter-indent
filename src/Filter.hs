{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
-- | Filtering a single code block.
module Filter(renderBlock, renderInline) where

import Text.Pandoc.JSON
import qualified Data.Text as T
import Prelude hiding(getLine)

import FindColumns ( tableColumns )
import Alignment ( Processed )
import Token ( MyTok )
import Render.ColSpan ( colspans )
import qualified Render.Debug
import qualified Render.Latex
import qualified Render.HTML

-- | Render a list of `Processed` token records into the target output format.
renderBlock ::  Format     -- ^ Format string
            ->  Attr       -- ^ Attributes
            -> [Processed] -- ^ Data about alignment
            ->  Block
--render "text" attrs aligned = RawBlock (Format "latex") $ processLatex aligned -- debug
renderBlock (Format "plain") attrs  = CodeBlock attrs           . Render.Debug.render
renderBlock (Format "latex") _attrs = RawBlock (Format "latex") . processLatex
renderBlock (Format "html" ) _attrs = RawBlock (Format "html" ) . processHTML
-- Debugging option
renderBlock other            attrs  = CodeBlock attrs           . T.pack . show

-- TODO: inline should strip colspans, and ignore table
-- | Render a list of `Processed` token records into the target output format.
renderInline ::  Format     -- ^ Format string
             ->  Attr       -- ^ Attributes
             -> [(MyTok, T.Text)] -- ^ Data about alignment
             ->  Inline
--render "text" attrs aligned = RawBlock (Format "latex") $ processLatex aligned -- debug
renderInline (Format "plain") attrs  = Code      attrs            . Render.Debug.renderInline
renderInline (Format "latex") _attrs = RawInline (Format "latex") . Render.Latex.latexInline
renderInline (Format "html" ) _attrs = RawInline (Format "html" ) . Render.HTML.htmlInline
-- Debugging option
renderInline other            attrs  = Code      attrs            . T.pack . show

-- | Convert a list of input token records to raw LaTeX.
processLatexInline :: _ -> T.Text
processLatexInline processed = Render.Latex.latexFromColSpans (length $ tableColumns processed)
                       $ colspans processed


-- | Convert a list of input token records to raw LaTeX.
processLatex :: [Processed] -> T.Text
processLatex processed = Render.Latex.latexFromColSpans (length $ tableColumns processed)
                       $ colspans processed

-- | Convert a list of input token records to raw HTML.
processHTML :: [Processed] -> T.Text
processHTML  = Render.HTML.htmlFromColSpans
             . colspans
