{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
module Render.ColSpan where

import Data.Function(on)
import Data.String (fromString, IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List(groupBy, sortBy)
import Data.Maybe(fromMaybe)
import Prelude hiding(getLine)
import Optics.Core ( Field1(_1), Field2(_2), view, (%), lens )
import Data.Tuple.Optics ( Field1(_1), Field2(_2) )

import Alignment
import FindColumns
import Token(MyTok)
import Util

type TokensWithColSpan = ([(MyTok, Text)], Int, Align)
--type TextWithColSpan = (Text, Int, [Processed])

--extractTexts :: [[TextWithColSpan]] -> [[_]]
{-extractTexts ps = fmap extractText <$> ps
  where
    maxCol :: Int
    maxCol = maximum $ map getAlignCol $ ps
    extractText (tok:toks, colspan) = (T.concat (view textContent <$> (tok:toks))
                                      ,colspan
                                      ,view (alignPos % maybeLens (ALeft,-1) % _1) tok)
 -}

-- | Find colspan parameters
colspans   :: [Processed] -> [[TokensWithColSpan]]
colspans ps = fmap ( fmap extractTokens
                   . addColSpans
                   . groupBy sameColSpan )
            $ groupBy ((==) `on` getLine)
            $ sortBy (compare `on` getLineCol) ps
  where
    maxCol :: Int
    maxCol = maximum $ map getAlignCol $ ps
    sameColSpan :: Processed -> Processed -> Bool
    sameColSpan tok1 tok2 = case view alignPos tok2 of
                              Nothing | getLine tok1 == getLine tok2 -> True
                              _                                      -> False
    addColSpans :: [[Processed]] -> [([Processed], Int, Align)]
    addColSpans []       = []
    addColSpans [a]      = [(a, maxCol -getAlignCol (head a)+1, getAlign $ head a)]
    addColSpans (b:c:cs) =  (b, nextCol-getAlignCol (head b)  , getAlign $ head b):addColSpans (c:cs)
      where
        nextCol = getAlignCol $ head c
    getAlign :: Processed -> Align
    getAlign = view (alignPos % maybeLens (ALeft, 0) % _1)
    extractTokens (a,b,c) = (extractToken <$> a, b, c)
    extractToken tok = (view tokenType tok, view textContent tok)
-- FIXME: split lines before colspans!

getAlignCol :: Processed -> Int
getAlignCol = view (alignPos % maybeLens (ALeft, 0) % _2)

