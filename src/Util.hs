{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
module Util where

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

import Token(MyTok(..))

-- | Lens with default value.
maybeLens :: a -> Lens (Maybe a) (Maybe a1) a a1
maybeLens dflt = lens (fromMaybe dflt) (\_ a -> Just a)

-- | Sort and group inputs by a given `Ordering`.
grouping    :: Ord k
            => (   a -> k)
            ->    [a]
            ->   [[a]]
grouping key = groupBy ((==)    `on` key)
             . sortBy  (compare `on` key)

nubSorted :: Ord a => [a] -> [a]
nubSorted  = nubSortedBy compare

-- | Given an `Ord`ering, sort and remove duplicates.
nubSortedBy :: (a -> a -> Ordering) -> [a] -> [a]
nubSortedBy comparison = fmap    head
                       . groupBy equality
                       . sortBy  comparison
  where
    equality a b = a `comparison` b == EQ

-- | Safe tail function that returns empty list for empty input.
safeTail []     = []
safeTail (_:ls) = ls

unbrace txt | T.head txt == '(' && T.last txt == ')' && T.length txt > 2 = Just $ T.tail $ T.init txt
unbrace _ = Nothing

preformatTokens []                                                     = []
preformatTokens ((TOperator,"`"):(TVar, "elem"):(TOperator, "`"):rest) = (TOperator, "elem"):preformatTokens rest
preformatTokens (a                                              :rest) =  a                 :preformatTokens rest

