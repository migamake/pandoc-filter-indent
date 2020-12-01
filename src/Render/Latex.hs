{-# LANGUAGE OverloadedStrings #-}
module Render.Latex(latexFromColSpans) where

import Data.Text(Text)
import Data.Char(isSpace)
import qualified Data.Text as T
import Text.LaTeX.Base.Syntax(protectText)

import Alignment
import Render.Common(TokensWithColSpan)
import Token(MyTok(..))

latexFromColSpans :: Int -> [[TokensWithColSpan]] -> Text
latexFromColSpans cols =
    wrapTable cols
  . T.unlines
  . fmap ( (<> "\\\\")
         . T.intercalate " & "
         . fmap renderColSpan )

renderColSpan :: TokensWithColSpan -> Text
renderColSpan ([(TBlank, txt)], colSpan, AIndent) = -- indentation
    T.concat [ "\\multicolumn{",    T.pack $ show colSpan
                          , "}{p{", T.pack $ show $ T.length txt
                          , "ex}}{",  protectText txt
                          , "}" ]
renderColSpan (toks, colSpan, alignment) =
    T.concat [ "\\multicolumn{",  T.pack $ show colSpan
                          , "}{", alignMark alignment
                          , "}{", formatTokens toks
                          , "}" ]
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

formatTokens :: [(MyTok, Text)] -> Text
formatTokens  = T.concat . map snd
