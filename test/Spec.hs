{-# LANGUAGE OverloadedStrings #-}
-- | Test suite
module Main where

import           Control.Exception(assert)
import           Data.Text.Arbitrary
import           Test.QuickCheck
import qualified Data.Text as T
import           Data.Char
import           Optics.Core (view, Field1(_1))


import Token(MyTok(..)
            ,MyLoc(..)
            ,Tokenized)
import Token.Haskell(tokenizer)
import Render.Latex(subscripts)
import qualified Render.Debug
import FindColumns
import Render.ColSpan
import Alignment(Processed)
import GHC.Stack

prop_tokenizer str = case tokenizer str of
                       Nothing -> label "cannot lex" $ True
                       Just t  -> label "lexed" $ length t <= T.length str

prop_debug_renderer_text_length input =
    case debug of
      Nothing -> label "cannot lex" $ True
      Just  t -> label "lexed"
               $ all (uncurry (<=))
               $ zip (extract input)
                     (extract t)
  where
    extract :: T.Text -> [Int]
    extract = fmap T.length . T.lines
    debug :: Maybe T.Text
    debug = tokenizer input >>= (return . Render.Debug.render . findColumns)

prop_colspans input =
    case debug of
      Nothing -> label "cannot lex" $ True
      Just  t -> label "lexed" $
                 sameNumber t
  where
    extract :: T.Text -> [Int]
    extract = fmap T.length . T.lines
    debug :: Maybe [Int]
    debug = tokenizer input >>= (return . sumColSpans . findColumns)
    sameNumber [] = True
    sameNumber (n:ns) = all (n==) ns

prop_tableColumns input = T.any (not . Data.Char.isSpace) input ==>
  case tokenizer input of
    Nothing     -> label "cannot lex" $ True
    Just tokens | all ((TBlank==) . view _1) tokens
                -> label "only blanks" $ True
    Just tokens -> 
      case findColumns tokens of
        [] -> label "empty" $ True
        t  -> case sumColSpans t of
                []  -> label "no colspans"  $ True
                c:_ -> label "tableColumns" $ c == length (tableColumns t)
  where
    extract :: T.Text -> [Int]
    extract = fmap T.length . T.lines
    debug :: Maybe [Processed]
    debug = tokenizer input >>= (return . findColumns)
    sameNumber [] = True
    sameNumber (n:ns) = all (n==) ns

shouldBe :: (HasCallStack, Eq a) => a -> a -> IO ()
a `shouldBe` b = do
  if a /= b 
    then error "Inequal"
    else return ()

main :: IO ()
main = do
    (subscripts "alpha_beta")        `shouldBe` ("alpha\\textsubscript{beta}")
    (subscripts "alpha__beta")       `shouldBe` ("alpha\\textsuperscript{beta}")
    (subscripts "alpha__gamma_beta") `shouldBe` ("alpha\\textsuperscript{gamma\\textsubscript{beta}}")
    problem " a\na"
    problem "--"
    problem "a\n a"
    quickCheck   prop_tokenizer
    quickCheck   prop_debug_renderer_text_length
    quickCheck $ withMaxSuccess 10000  prop_colspans -- is it sufficient?
    quickCheck $ withMaxSuccess 100000 prop_tableColumns -- is it sufficient?
  where
    problem example = do
      putStrLn $ "Example: " <> show example
      print $ tokenizer example >>= (return . tableColumns . findColumns)
      print $ tokenizer example >>= (return . sumColSpans  . findColumns)
      print $ tokenizer example >>= (return . findColumns)
      print $ tokenizer example

sumColSpans = map (sum . map (\(_,c,_) -> c)) . colspans

