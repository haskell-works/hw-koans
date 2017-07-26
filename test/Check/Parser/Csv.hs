{-# LANGUAGE TemplateHaskell #-}

module Check.Parser.Csv where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Hedgehog
import Hedgehog.Extra

import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range
import qualified Koan.Parser.Csv      as K
import qualified Check.Parser.Csv.Ref as R

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

genTextData :: MonadGen m => m Char
genTextData = chr <$> Gen.choice
  [ pure 0x20
  , pure 0x21
  , Gen.enum 0x23 0x2B
  , Gen.enum 0x2D 0x7E
  ]

genEscapable :: MonadGen m => m String
genEscapable = Gen.choice
  [ pure "\"\""
  , (:[]) <$> Gen.choice
    [ genTextData
    , pure '\n'
    , pure '\r'
    ]
  ]

noTrailing :: [[String]] -> [[String]]
noTrailing = reverse . dropWhile (== [""]) . reverse

prop_csv_simple :: Property
prop_csv_simple = property $ do
  let genField  = Gen.list (Range.linear 1 5) genTextData
  let genRecord = concat <$> (intersperse "," <$> Gen.list (Range.linear 1 5) genField)
  let genFile   = concat <$> (intersperse "\n" <$> Gen.list (Range.linear 1 5) genRecord)
  text <- forAll genFile
  (noTrailing <$> K.parseCsv text) === (noTrailing <$> R.parseCsv text)

prop_csv_escaped :: Property
prop_csv_escaped = property $ do
  let genField  = concat <$> Gen.list (Range.linear 1 5) genEscapable
  let genRecord = concat <$> (intersperse "," <$> Gen.list (Range.linear 1 5) genField)
  let genFile   = concat <$> (intersperse "\n" <$> Gen.list (Range.linear 1 5) genRecord)
  text <- forAll genFile
  (noTrailing <$> K.parseCsv text) === (noTrailing <$> R.parseCsv text)

prop_csv_escaped_with_trailing_newline :: Property
prop_csv_escaped_with_trailing_newline = property $ do
  let genField  = concat <$> Gen.list (Range.linear 1 5) genEscapable
  let genRecord = concat <$> (intersperse "," <$> Gen.list (Range.linear 1 5) genField)
  let genFile   = concat <$> (intersperse "\n" <$> Gen.list (Range.linear 1 5) genRecord)
  text <- forAll $ do
    body              <- genFile
    trailingNewlines  <- Gen.list (Range.linear 0 2) (pure ' ')
    pure (body ++ trailingNewlines)
  K.parseCsv text === R.parseCsv text

prop_csv_empty_file :: Property
prop_csv_empty_file = property $ do
  K.parseCsv "" === Right []

prop_csv_empty_line :: Property
prop_csv_empty_line = property $ do
  K.parseCsv "\n" === Right [[""]]

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
