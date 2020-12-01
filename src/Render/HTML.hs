{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Render.HTML(htmlFromColSpans) where

import Prelude hiding(span)
import Data.Text(Text)
import Data.Char(isSpace)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Text.Blaze.Html5 hiding(style)
import Text.Blaze.Html5.Attributes(colspan, style)
import Text.Blaze.Html.Renderer.Text(renderHtml)

import Alignment
import Render.Common(TokensWithColSpan)
import Token(MyTok(..))
import Debug.Trace(trace)
import Util

--latexFromColSpans :: Int -> [[TokensWithColSpan]] -> Text
htmlFromColSpans cols =
    LT.toStrict
  . renderHtml
  . table
  . tbody
  . mapM_ renderTr

renderTr colspans = tr (mapM_ renderColSpan colspans)

--renderColSpan :: TokensWithColSpan -> Text
renderColSpan ([(TBlank, txt)], colSpan, AIndent) = -- indentation
    (td $ toHtml txt)
        ! colspan (toValue colSpan)
        ! style   widthStyle
  where
    widthStyle = toValue $ "min-width: " <> show (T.length txt) <> "ex"
renderColSpan (toks, colSpan, alignment) =
    (td $ formatTokens toks)
        ! colspan (toValue colSpan)
        ! style   alignStyle
  where
    alignStyle = "text-align: " <> alignMark alignment
    alignMark ACenter = "center"
    alignMark ALeft   = "left"

-- Decrease column spacing: \\setlength{\\tabcolsep}{1ex}
-- TODO: braced operators
formatTokens :: [(MyTok, Text)] -> Html
formatTokens  = mapM_ formatToken
              . preformatTokens

formatToken :: (MyTok, Text) -> Html
formatToken (TOperator,unbrace -> Just op) = do "("
                                                formatToken (TOperator, op)
                                                ")"
formatToken (TOperator,"|>"       ) = "⊳"
formatToken (TOperator,"<>"       ) = "⋄"
formatToken (TOperator,"=>"       ) = "⇒"
formatToken (TOperator,"->"       ) = "→"
formatToken (TOperator,"|->"       ) = "↦"
formatToken (TVar     ,"undefined") = "⊥"
formatToken (TVar     ,"bot"      ) = "⊥"
formatToken (TVar     ,"not"      ) = "¬"
formatToken (TVar     ,"a"        ) = "α"
formatToken (TVar     ,"b"        ) = "β"
formatToken (TVar     ,"c"        ) = "γ"
formatToken (TVar     ,"d"        ) = "δ"
formatToken (TVar     ,"pi"       ) = "π"
formatToken (TVar     ,"eps"      ) = "ε"
formatToken (TKeyword ,"\\"       ) = "λ"
formatToken (TKeyword, "forall"   ) = "∀"
formatToken (TOperator,"elem"     ) = "∈"
formatToken (TOperator,"<="       ) = "≤"
formatToken (TOperator,">="       ) = "≥"
formatToken (TOperator,"mempty"   ) = "∅"
formatToken (TOperator,">>>"      ) = "⋙"
formatToken (TOperator,"<<<"      ) = "⋘"
formatToken (TOperator,"||"       ) = "∥"
formatToken (TOperator,"<->"      ) = "↔︎"
formatToken (TOperator,"<-"      ) = "←"
formatToken (TOperator,"-<"       ) = "≺"
formatToken (TOperator,">-"       ) = "≻"
formatToken (TOperator,"!="       ) = "≠"
formatToken (TOperator,"\\/"       ) = "⋁"
formatToken (TOperator,"/\\"       ) = "⋀"
formatToken (TOperator,"~"       ) = "∼"
formatToken (TOperator,"~="       ) = "≈"
formatToken (TVar,"top"       ) = "⊤"
formatToken (TKeyword, kwd   ) = b $ toHtml kwd
formatToken (TVar,     v     ) = i $ toHtml v
formatToken (TCons,     v    ) = span (toHtml v)
                                      ! style ("font-variant: small-caps;")
formatToken (_, txt) = toHtml txt

