{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
module Filter where

import Text.Pandoc.JSON
import Text.Pandoc.Definition ()
import Data.Function(on)
import Data.String (fromString, IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List(groupBy, sortBy, sort, group)
import Data.Maybe(fromMaybe)
import Prelude hiding(getLine)
import Optics.Core
import Data.Tuple.Optics
import Data.Tuple.Optics
import Data.Text.Lazy.Builder

import Debug.Trace(trace)

import Token
import Token.Haskell
import Tuples
import FindColumns
import Alignment
import Util

filterCodeBlock = withTokens findColumns ("haskell", tokenizer)

-- | Apply function if tokenization succeeded, otherwise return same output
withTokens f (tokenizerName, tokenizer) src@(tokenizer -> Nothing) = error $ mconcat ["Tokenizer ", tokenizerName, " failed for ", show src]
withTokens f (tokenizerName, tokenizer)     (tokenizer -> Just tokens) = f tokens

render ::  Text       -- ^ Format string
       ->  Attr       -- ^ Attributes
       -> [Processed] -- ^ Data about alignment
       ->  Block
render "text" attrs aligned = trace "Rendering as text" $ CodeBlock attrs $ renderText2 aligned
render other  attrs aligned = CodeBlock attrs $ T.pack $ show aligned

-- | Find colspan parameters
colspans   :: [Processed] -> [([Processed], Int)]
colspans ps = addColSpans
            $ groupBy sameColSpan
            $ sortBy (compare `on` getLineCol) ps
  where
    maxCol :: Int
    maxCol = maximum $ map getAlignCol ps
    sameColSpan :: Processed -> Processed -> Bool
    sameColSpan tok1 tok2 = case view alignPos tok2 of
                              Nothing | getLine tok1 == getLine tok2 -> True
                              _                                      -> False
    addColSpans :: [[Processed]] -> [([Processed], Int)]
    addColSpans []       = []
    addColSpans [a]      = [(a, maxCol -getAlignCol (head a)+1)]
    addColSpans (b:c:cs) =  (b, nextCol-getAlignCol (head b)  ):addColSpans (c:cs)
      where
        nextCol = getAlignCol $ head c
    getAlignCol = view (alignPos % maybeLens (ALeft, 0) % _2)
    maybeLens dflt = lens (fromMaybe dflt) (\_ a -> Just a)
-- FIXME: split lines before colspans!

-- FIXME: add spaces in-between
renderText :: [Processed] -> Text
renderText  = T.concat
            . go
            . (0, 0,)
  where
    go (currentLine, currentCol, []      ) = []
    go (currentLine, currentCol, tok:toks) =
        alignMarker:view textContent tok:go (getLine tok, getCol tok, toks)
      where
        alignMarker :: Text
        alignMarker  = case view alignPos tok of
                         Nothing           -> ""
                         Just (ACenter, _) -> "^"
                         Just (ALeft,   _) -> "|"

insertAt :: Show a => Int -> a -> [a] -> [a]
insertAt i e ls = case maybeInsertAt i e ls of
                    Nothing -> error $ "Failed in insertAt " <> show i <> " " <> show e <> " " <> show ls
                    Just r  -> r

maybeInsertAt 0 e    ls  = pure $ e:ls
maybeInsertAt i e (l:ls) = (l:) <$> maybeInsertAt (i-1) e ls
maybeInsertAt i e    []  = Nothing


renderText2 :: [Processed] -> Text
renderText2 ps = T.concat $ go ps
  where
    -- | Table columns between starting point of this segment and the next.
    lastColumn  = maximum tColumns + 1
    -- | Translates indices of alignment/table columns to text columns.
    tColumns :: [Int]
    tColumns = fst <$> tableColumns ps
    go  []        = []
    go (tok:toks) = alignMarker:textWithMarkers tColumns nextCol tok
                               :go toks
      where
        nextCol = case toks of
                    []       -> lastColumn
                    (next:_) -> getCol next
        alignMarker :: Text
        alignMarker  = case view alignPos tok of
                         Nothing           -> ""
                         Just (ACenter, _) -> "^"
                         Just (ALeft,   _) -> "|"

-- | Text content with markers for markers inside it.
textWithMarkers tColumns nextCol tok = 
    T.pack $ insertMarkers unaccountedMarkers $ T.unpack $ view textContent tok
  where
    insertMarkers []   txt = txt
    insertMarkers mrks txt = (\result -> trace ("insertMarkers " <> show mrks <> " " <> show txt <> " => " <> result) result)
                           $ foldr insertMarker txt
                           $ trace (show unaccountedMarkers) mrks

    insertMarker index = insertAt index '.'
    unaccountedMarkers = fmap (-getCol tok+)
                       $ withoutKnownMarker
                       $ columnsBetween (getCol tok) nextCol
    withoutKnownMarker | Just _ <- view alignPos tok = safeTail
                       | otherwise                   = id
    columnsBetween :: Int -> Int -> [Int]
    columnsBetween colA colB = filter (\c -> c >= colA && c < colB) tColumns
