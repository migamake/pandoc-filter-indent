{-# LANGUAGE OverloadedStrings #-}
module Render.Latex(latexFromColSpans) where

import Data.Text(Text)
import Data.Char(isSpace)
import qualified Data.Text as T
import Text.LaTeX.Base.Syntax(protectText)

import Alignment
import Render.Common(TokensWithColSpan)
import Token(MyTok(..))
import Debug.Trace(trace)

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
formatTokens  = T.concat . fmap formatToken . (\t -> trace ("Tokens: " <> show t) t)

formatToken (TKeyword, kwd     ) = "\\textbf{" <> protectText kwd <> "}"
formatToken (TOther,   ">>="   ) = mathop "gg\\joinrel="
formatToken (TOther,   "forall") = mathop "forall"
formatToken (TOther,   "mempty") = mathop "emptyset"
formatToken (TOther,   "bottom") = mathop "bot"
formatToken (TOther,   "top"   ) = mathop "top"
formatToken (TOther,   "not"   ) = mathop "neg"
formatToken (TOther,   "|"     ) = mathop "vert"
formatToken (TOther,   "||"    ) = mathop "parallel"
formatToken (TOther,   "|>"    ) = mathop "triangleright"
formatToken (TOther,   ">>"    ) = mathop "gg"
formatToken (TOther,   ">>>"   ) = mathop "ggg"
formatToken (TOther,   "<<"    ) = mathop "ll"
formatToken (TOther,   "<<<"   ) = mathop "lll"
formatToken (TOther,   "-<"    ) = mathop "prec"
formatToken (TOther,   "<-"    ) = mathop "gets"
formatToken (TOther,   ">="    ) = mathop "geq"
formatToken (TOperator,"<="    ) = mathop "leq"
formatToken (TOperator,"!="    ) = mathop "ne"
formatToken (TOperator,"<->"   ) = mathop "leftrightarrow"
formatToken (TOperator,"->"    ) = mathop "rightarrow"
formatToken (TOperator,"=>"    ) = mathop "Rightarrow"
formatToken (TOperator,"<>"    ) = mathop "diamond"
formatToken (TOperator,"elem"  ) = mathop "in"
formatToken (TOperator,"~"     ) = mathop "sim"
formatToken (TOperator,"~="    ) = mathop "approx"
formatToken (TOther,   "mempty") = mathop "gg"
formatToken (TOther,   "a"     ) = mathop "alpha"
formatToken (TOther,   "b"     ) = mathop "beta"
formatToken (TOther,   "\\"    ) = mathop "lambda"
formatToken (_, txt) = txt

mathop code = "\\ensuremath{\\" <> code <> "{}}"
