{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
-- | Render analyzed input into LaTeX table.
module Render.Latex(latexFromColSpans) where

import Data.Text(Text)
import qualified Data.Text as T
import Text.LaTeX.Base.Syntax(protectText)

import Alignment
import Render.Common(TokensWithColSpan)
import Token(MyTok(..))
import Util(unbrace)

-- | Given a number of table columns,
--   and a list of lists of colspans for each table row,
--   return raw LaTeX code.
latexFromColSpans :: Int -> [[TokensWithColSpan]] -> Text
latexFromColSpans cols =
    wrapTable cols
  . T.unlines
  . fmap ( (<> "\\\\")
         . T.intercalate " & "
         . fmap renderColSpan )

-- | Render a single colspan as LaTeX \multicolumn.
renderColSpan :: TokensWithColSpan -> Text
renderColSpan ([(TBlank, txt)], colSpan, AIndent) = -- indentation
    T.concat [ "\\multicolumn{",    T.pack $ show colSpan
                          , "}{p{", T.pack $ show $ T.length txt
                          , "ex}}{",  protectText txt
                          , "}" ]
renderColSpan (toks, colSpan, alignment) =
    T.concat [ "\\multicolumn{",  T.pack $ show colSpan
                          , "}{", alignMark alignment
                          , "}{$", formatTokens toks
                          , "$}" ]
  where
    alignMark ACenter = "c"
    alignMark ALeft   = "l"

-- | Wrap a LaTeX table content into \begin{tabular} environment.
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
-- | Preprocesses functions converted to operator syntax and joins them into a single token.
-- FIXME: deduplicate
preformatTokens []                                                     = []
preformatTokens ((TOperator,"`"):(TVar, "elem"):(TOperator, "`"):rest) = (TOperator, "elem"):preformatTokens rest
preformatTokens (a                                              :rest) =  a                 :preformatTokens rest

-- | Format a list of tokens within a colspan.
--   Preprocesses then and calls `formatToken` for each.
formatTokens :: [(MyTok, Text)] -> Text
formatTokens  = T.concat
              . fmap formatToken
              . preformatTokens
              -- . (\t -> trace ("Tokens: " <> show t) t) -- debug

-- Workaround with joinEscapedOperators til w consider spaces only.
-- | Render a simple token.
formatToken :: (MyTok, Text) -> Text
formatToken (TOperator,unbrace -> Just op) = "(" <> formatToken (TOperator, op) <> ")"
formatToken (TKeyword, "forall") = mathop "forall"
formatToken (TVar,     "mempty") = mathop "emptyset"
formatToken (TVar,     "bottom") = mathop "bot"
formatToken (TVar,  "undefined") = mathop "perp"
formatToken (TVar,     "top"   ) = mathop "top"
formatToken (TVar,     "not"   ) = mathop "neg"
formatToken (TOperator,">>="   ) = mathop "mathbin{>\\!\\!\\!>\\!\\!=}" -- from lhs2TeX, Neil Mitchell's
formatToken (TOperator,"=<<"   ) = mathop "mathbin{=\\!\\!<\\!\\!\\!<}" -- from lhs2TeX, Neil Mitchell's
formatToken (TOperator,">=>"   ) = mathop "mathbin{>\\!\\!=\\!\\!\\!>}"
formatToken (TOperator,"|-"    ) = mathop "vdash"
formatToken (TOperator,"/\\"   ) = mathop "lor"
formatToken (TOperator,"\\/"   ) = mathop "land"
formatToken (TOperator,"\\|/"  ) = mathop "downarrow"
formatToken (TOperator,"\\||/" ) = mathop "Downarrow"
formatToken (TOperator,"~>"    ) = mathop "leadsto"
formatToken (TOperator,"|="    ) = mathop "models"
formatToken (TCons    ,"Natural") = mathop "N"
formatToken (TOperator,"|"     ) = mathop "vert"
formatToken (TOperator,"||"    ) = mathop "parallel"
formatToken (TOperator,"|>"    ) = mathop "triangleright"
formatToken (TOperator,">>"    ) = mathop "mathbin{>\\!\\!\\!>}" -- gg
formatToken (TOperator,">>>"   ) = mathop "mathbin{>\\!\\!\\!>\\!\\!\\!>}" -- gg
formatToken (TOperator,">>"    ) = mathop "gg"
formatToken (TOperator,">>>"   ) = mathop "ggg"
formatToken (TOperator,"<<"    ) = mathop "ll"
formatToken (TOperator,"<<<"   ) = mathop "lll"
formatToken (TOperator,"-<"    ) = mathop "prec"
formatToken (TOperator,"\\\\"  ) = mathop "setminus"
formatToken (TOperator,"<-"    ) = mathop "gets"
formatToken (TOperator,">="    ) = mathop "geq"
formatToken (TOperator,"<="    ) = mathop "leq"
formatToken (TOperator,"!="    ) = mathop "ne"
formatToken (TOperator,"<->"   ) = mathop "leftrightarrow"
formatToken (TOperator,"->"    ) = mathop "to"
formatToken (TOperator,"=>"    ) = mathop "Rightarrow"
formatToken (TOperator,"==>"   ) = mathop "implies"
formatToken (TOperator,"|->"   ) = mathop "mapsto"
--formatToken (TOperator,"|=>"   ) = mathop "Mapsto" -- not in amssymb
formatToken (TOperator,"<>"    ) = mathop "diamond"
formatToken (TOperator,"<$>"   ) = mathop "mathbin{\\ooalign{\\raise.29ex\\hbox{$\\scriptscriptstyle\\$$}\\cr\\hss$\\!\\lozenge$\\hss}}"
formatToken (TOperator,"<*>"   ) = mathop "mathbin{\\ooalign{\\raise.37ex\\hbox{$\\scriptscriptstyle{*}$}\\cr\\hss$\\!\\lozenge$\\hss}}"
formatToken (TOperator,"elem"  ) = mathop "in"
formatToken (TOperator,"~"     ) = mathop "sim"
formatToken (TOperator,"~="    ) = mathop "approx"
formatToken (TVar,     "a"     ) = mathop "alpha"
formatToken (TVar,     "b"     ) = mathop "beta"
formatToken (TVar,     "c"     ) = mathop "gamma"
formatToken (TVar,     "d"     ) = mathop "delta"
formatToken (TVar,     "eps"   ) = mathop "epsilon"
formatToken (TVar    , kwd     ) = "\\emph{"       <> protectText kwd  <> "}"
formatToken (TNum    , kwd     ) = protectText kwd 
formatToken (TKeyword, kwd     ) = "\\textbf{"     <> protectText kwd  <> "}"
formatToken (TCons,    cons    ) = "\\textsc{"     <> protectText cons <> "}"
formatToken (TOperator,"\\"    ) = mathop "lambda"
formatToken (TTikz mark,_      ) = mathop $ "tikzMark{" <> mark <> "}"
formatToken (_,        txt     ) = "\\textit{"     <> protectText txt  <> "}"

mathop code = "\\" <> code

prologue = T.concat ["\\usepackage{amssymb}"]
