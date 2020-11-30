{-# LANGUAGE OverloadedStrings #-}
module Render.Latex(latexFromColSpans) where

import Data.Text(Text)
import Data.Char(isSpace)
import qualified Data.Text as T
import Text.LaTeX.Base.Syntax(protectText)

import Alignment
import Render.Common(TextWithColSpan)

latexFromColSpans :: Int -> [[TextWithColSpan]] -> Text
latexFromColSpans cols =
    wrapTable cols
  . T.unlines
  . fmap ( (<> "\\\\")
         . T.intercalate " & "
         . fmap renderColSpan )

renderColSpan :: TextWithColSpan -> Text
renderColSpan (text, colSpan, AIndent) | T.all isSpace text =
    T.concat [ "\\multicolumn{",    T.pack $ show colSpan
                          , "}{p{", T.pack $ show $ T.length text
                          , "ex}}{",  protectText text
                          , "}"]
renderColSpan (text, colSpan, alignment) =
    T.concat [ "\\multicolumn{",  T.pack $ show colSpan
                          , "}{", alignMark alignment
                          , "}{", protectText text
                          , "}"]
  where
    alignMark ACenter = "c"
    alignMark ALeft   = "l"

wrapTable :: Int -> Text -> Text
wrapTable cols txt =
  mconcat [-- "\\newlength{\\tabcolsepBACKUP}\n"
           -- ,"\\setlength{\\tabcolsepBACKUP}{\\tabcolsep}"
            "\\setlength{\\tabcolsep}{1pt}\n"
          , "\\begin{tabular}{"
          , T.replicate (cols+2) "l"
          , "}\n"
          , txt, "\n\\end{tabular}"
           --,"\\setlength{\\tabcolsep}{\\tabcolsepBACKUP}"
          ]

-- Decrease column spacing: \\setlength{\\tabcolsep}{1ex}
