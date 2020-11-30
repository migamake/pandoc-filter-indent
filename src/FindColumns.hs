{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
module FindColumns(findColumns, getLine, getCol, getLineCol, alignPos, tableColumns) where

import Text.Pandoc.JSON
import Text.Pandoc.Definition ()
import Data.Function  (on)
import Data.String    (fromString, IsString)
import Data.Text      (Text)
import Data.List      (groupBy, sortBy, sort, group, findIndex)
import Prelude hiding (getLine)
import Optics.Core
import Data.Tuple.Optics

import Token
import Token.Haskell
import Tuples
import Alignment
import Util

-- | Records tokenized and converted to common token format.
type Unanalyzed = (MyTok, MyLoc, Text, Maybe Int             )

-- | Records aligned, but without column numbers yet.
type Aligned   = (MyTok, MyLoc, Text, Maybe Int, Maybe Align)

-- * Definitions of fields accessible at many stages
getCol, getLine :: Field2 a a MyLoc MyLoc => a -> Int
getCol  = view $ _2 % col
getLine = view $ _2 % line

align :: Field5 a a (Maybe b) (Maybe b) => Lens' a (Maybe b)
align  = _5

alignPos :: Field5 a a (Maybe (Align, Int)) (Maybe (Align, Int)) => Lens' a (Maybe (Align, Int))
alignPos  = _5

-- | Find columns from tokens.
findColumns :: [Tokenized] -> [Processed]
findColumns =
      columnIndices
    . withExtraColumns
    . sortBy (compare `on` getLineCol)
    . concat
    . map markBoundaries
    . grouping getCol
    . concat
    . map addLineIndent
    . grouping getLine

getLineCol x = (getLine x, getCol x)

markBoundaries :: [Unanalyzed] -> [Aligned]
markBoundaries = map markIndent
               . concat
               . map alignBlock
               . blocks
               . sortBy (compare `on` getLine)
  where
    getLine (tok, MyLoc line col, _, _) = line

-- | If first indented token is yet unmarked, mark it as boundary.
markIndent :: Aligned -> Aligned
markIndent (myTok, myLoc@(MyLoc _ col), txt, Just indent, Nothing   ) | indent == col =
           (myTok, myLoc              , txt, Just indent, Just ALeft)
markIndent other                                                               = other

withAlign :: Maybe Align -> Unanalyzed -> Aligned
withAlign  = flip annex

alignBlock :: [Unanalyzed] -> [Aligned]
alignBlock [a]                            = withAlign  Nothing       <$> [a]
alignBlock opList | all isOperator opList = withAlign (Just ACenter) <$> opList
  where
    isOperator (TOperator, _, _, _) = True
    isOperator  _                   = False
alignBlock aList                          = withAlign (Just ALeft  ) <$> aList

-- | Compute all alignment columns existing and their positions in the text column space.
extraColumns :: Field2 a a  MyLoc         MyLoc
             => Field5 a a (Maybe Align) (Maybe Align)
             => [a] -> [(Int, Maybe Align)]
extraColumns =
    nubSorted
  . filter hasAlignment
  . map extract
  where
    extract x = (getCol x, view align x)
    hasAlignment (_, Nothing) = False
    hasAlignment (_, Just _)  = True

withExtraColumns x = (x, extraColumns x)

-- | Compute all alignment columns existing and their positions in the text column space.
tableColumns :: Field2 a a  MyLoc     MyLoc
             => Field5 a a (Maybe b) (Maybe b)
             => [a]
             -> [(Int, Maybe b)]
tableColumns  =
    nubSortedBy (compare `on` fst)
  . filter hasAlignment
  . map extract
  where
    extract x                 = (getCol x, view align x)
    hasAlignment (_, Nothing) = False
    hasAlignment (_, Just _)  = True

{-
-- | Detect uninterrupted stretches that cover consecutive columns.
blocks :: [Unanalyzed] -> [[Unanalyzed]]
blocks (b:bs) = go [b] bs
  where
    getLine (_, MyLoc line _, _, _) = line
    go currentBlock@(getLine . head -> lastCol) (b@(getLine -> nextCol):bs)
      | nextCol - lastCol == 1 = -- add ignoring of unindented (Nothing)
        go (b:currentBlock)         bs
    go (c:currentBlock)                         (blank@(_,_,Nothing, _):bs) =
        go (c:blank:currentBlock)   bs
    go currentBlock                             (b                     :bs) =
        reverse currentBlock:go [b] bs
    go currentBlock                             []                          =
       [reverse currentBlock]
 -}

-- | Detect uninterrupted stretches that cover consecutive columns.
blocks :: [Unanalyzed] -> [[Unanalyzed]]
blocks = groupBy consecutive
  where
    consecutive :: Unanalyzed -> Unanalyzed -> Bool
    consecutive (getLine -> lastLine) (getLine -> nextLine) | nextLine - lastLine == 1 = True
      where
        getLine (_, MyLoc line _, _, _) = line
    consecutive  _                    _                                            = False

--withGroups k f = map k . grouping k

-- | Add line indent to each token in line.
addLineIndent :: [Tokenized] -> [Unanalyzed]
addLineIndent aLine = (`annex` indentColumn) <$> aLine
  where
    indentColumn :: Maybe Int
    indentColumn = extractColumn $ filter notBlank aLine
    notBlank       (TBlank, _, _)               = False
    notBlank        _                           = True
    extractColumn  []                           = Nothing
    extractColumn  ((_,   MyLoc line col, _):_) = Just col

columnIndices :: ([Aligned], [(Int, b)]) -> [Processed]
columnIndices (allAligned, map fst -> markerColumns) = map addIndex allAligned
  where
    addIndex :: Aligned -> Processed
    addIndex aligned = (_5 `over` mod) aligned
      where
        mod :: Maybe Align -> Maybe (Align, Int)
        mod Nothing   = Nothing
        mod (Just  a) =
             Just (a, colIndex)
          where
            colIndex = case findIndex (==getCol aligned) markerColumns of
                         Nothing -> error $ "Did not find the index for column " <> show (getCol aligned) <> " within " <> show markerColumns
                         Just i  -> i

