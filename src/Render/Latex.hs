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
          , T.replicate (cols+3) "l" -- FIXME: tests for correct number of columns
          , "}\n"
          , txt, "\n\\end{tabular}"
           --,"\\setlength{\\tabcolsep}{\\tabcolsepBACKUP}"
          ]

-- Decrease column spacing: \\setlength{\\tabcolsep}{1ex}
-- TODO: braced operators
preformatTokens []                                                     = []
preformatTokens ((TOperator,"`"):(TVar, "elem"):(TOperator, "`"):rest) = (TOperator, "elem"):preformatTokens rest
preformatTokens (a                                              :rest) =  a                 :preformatTokens rest

formatTokens :: [(MyTok, Text)] -> Text
formatTokens  = T.concat
              . fmap formatToken
              . preformatTokens
              . (\t -> trace ("Tokens: " <> show t) t) -- debug

formatToken (TKeyword, "forall") = mathop "forall"
formatToken (TVar,     "mempty") = mathop "emptyset"
formatToken (TVar,     "bottom") = mathop "bot"
formatToken (TVar,     "top"   ) = mathop "top"
formatToken (TVar,     "not"   ) = mathop "neg"
--formatToken (TOperator,"("     ) = mathop "("
--formatToken (TOperator,")"     ) = mathop ")"
formatToken (TOperator,">>="   ) = mathop "gg\\joinrel="
formatToken (TOperator,">=>"   ) = mathop ">\\joinrel=>"
formatToken (TOperator,"|"     ) = mathop "vert"
formatToken (TOperator,"||"    ) = mathop "parallel"
formatToken (TOperator,"|>"    ) = mathop "triangleright"
formatToken (TOperator,">>"    ) = mathop "gg"
formatToken (TOperator,">>>"   ) = mathop "ggg"
formatToken (TOperator,"<<"    ) = mathop "ll"
formatToken (TOperator,"<<<"   ) = mathop "lll"
formatToken (TOperator,"-<"    ) = mathop "prec"
formatToken (TOperator,"<-"    ) = mathop "gets"
formatToken (TOperator,">="    ) = mathop "geq"
formatToken (TOperator,"<="    ) = mathop "leq"
formatToken (TOperator,"!="    ) = mathop "ne"
formatToken (TOperator,"<->"   ) = mathop "leftrightarrow"
formatToken (TOperator,"->"    ) = mathop "rightarrow"
formatToken (TOperator,"=>"    ) = mathop "Rightarrow"
formatToken (TOperator,"<>"    ) = mathop "diamond"
formatToken (TOperator,"elem"  ) = mathop "in"
formatToken (TOperator,"~"     ) = mathop "sim"
formatToken (TOperator,"~="    ) = mathop "approx"
formatToken (TVar,     "a"     ) = mathop "alpha"
formatToken (TVar,     "b"     ) = mathop "beta"
formatToken (TVar,     "c"     ) = mathop "gamma"
formatToken (TVar,     "d"     ) = mathop "delta"
formatToken (TVar,     "eps"   ) = mathop "epsilon"
formatToken (TVar    , kwd     ) = "\\emph{" <> protectText kwd <> "}"
formatToken (TNum    , kwd     ) = "\\ensuremath{" <> protectText kwd <> "}"
formatToken (TKeyword, kwd     ) = "\\textbf{" <> protectText kwd <> "}"
formatToken (TCons,    cons    ) = "\\textsc{" <> protectText cons <> "}"
formatToken (TOperator,"\\"    ) = mathop "lambda"
formatToken (_,        txt     ) = protectText txt

mathop code = "\\ensuremath{\\" <> code <> "{}}"
