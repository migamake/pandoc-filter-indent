{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
module FindColumns(findColumns) where

import Text.Pandoc.JSON
import Text.Pandoc.Definition ()
import Data.Function(on)
import Data.String (fromString, IsString)
import Data.Text (Text)
import Data.List(groupBy, sortBy, sort, group)
import Prelude hiding(getLine)
import Optics.Core
import Data.Tuple.Optics

import Token
import Token.Haskell
import Tuples
import Alignment

-- | Records tokenized and converted to common token format.
type Unanalyzed = (MyTok, MyLoc, Text, Maybe Int             )

-- * Definitions of fields accessible at many stages
getCol, getLine :: Field2 a a MyLoc MyLoc => a -> Int
getCol  = view $ _2 % col
getLine = view $ _2 % line

align :: Field5 a a (Maybe Align) (Maybe Align) => Lens' a (Maybe Align)
align  = _5

-- | Find columns from tokens.
findColumns :: [Tokenized] -> [Aligned]
findColumns =
      markSpaces
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

nubSorted = fmap head
          . group
          . sort

withExtraColumns x = (x, extraColumns x)

markSpaces (tokens, spaces) = markSpace (view _1 <$> spaces) <$> tokens

markSpace :: [Int] -- ^ All columns with alignment markers
          -> _
          -> _
markSpace spaces token =
  case view align token of
    Nothing | getCol token `elem` spaces -> -- Check if there is space to be inserted
      set align (Just ANone) token
    _              -> token
  
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

grouping    :: Ord k
            => (   a -> k)
            ->    [a]
            ->   [[a]]
grouping key = groupBy ((==)    `on` key)
             . sortBy  (compare `on` key)

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

