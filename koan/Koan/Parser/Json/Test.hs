{-# LANGUAGE TemplateHaskell #-}

module Koan.Parser.Json.Test where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Char
import Data.Either
import Data.List
import Hedgehog
import Text.Megaparsec
import HaskellWorks.Hedgehog
import Hedgehog.Internal.Property (MonadTest(..), failDiff)
import Hedgehog.Internal.Source (HasCallStack(..), withFrozenCallStack)

import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range
import qualified Koan.Parser.Json     as K

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

genBool :: MonadGen m => m Bool
genBool = Gen.bool

genString :: MonadGen m => m String
genString = Gen.string (Range.constant 0 8) Gen.alpha

genObject :: MonadGen m => Int -> m [(String, K.Json)]
genObject n = Gen.list (Range.linear 0 2) (genField (n - 1))

genArray :: MonadGen m => Int -> m [K.Json]
genArray n = Gen.list (Range.linear 0 2) (genJson' (n - 1))

genNumber :: MonadGen m => m Double
genNumber = Gen.double (Range.linearFrac (-100.0) 100.0)

genJson :: MonadGen m => m K.Json
genJson = genJson' 3

genField :: MonadGen m => Int -> m (String, K.Json)
genField n = (,) <$> Gen.string (Range.constant 0 4) Gen.alpha <*> genJson' n

genJson' :: MonadGen m => Int -> m K.Json
genJson' n | n <= 0 = Gen.choice
  [ pure   K.JsonNull
  , K.JsonBool   <$> genBool
  , K.JsonNumber <$> genNumber
  ]
genJson' n = Gen.choice
  [ pure   K.JsonNull
  , K.JsonBool   <$> genBool
  , K.JsonNumber <$> genNumber
  , K.JsonArray  <$> genArray n
  , K.JsonObject <$> genObject n
  , K.JsonString <$> genString
  ]

toString2 :: (String, K.Json) -> String
toString2 (field, value) = show field ++ ":" ++ toString value

toString :: K.Json -> String
toString (K.JsonBool    v) = if v then "true" else "false"
toString (K.JsonNumber  v) = show v
toString (K.JsonString  v) = show v
toString (K.JsonArray   v) = "[" ++ intercalate "," (toString  <$> v) ++ "]"
toString (K.JsonObject  v) = "{" ++ intercalate "," (toString2 <$> v) ++ "}"
toString  K.JsonNull       = "null"

prop_comma_matched :: Property
prop_comma_matched = property $ do
  parse K.comma "" "," === Right ()

prop_comma_unmatched :: Property
prop_comma_unmatched = property $ do
  nonComma <- forAll $ (/= ',') `Gen.filter` Gen.ascii
  parse K.comma "" [nonComma] /== Right ()

prop_plainChar_matched :: Property
prop_plainChar_matched = property $ do
  nonEscapee <- forAll $ (`notElem` K.escapees) `Gen.filter` Gen.ascii
  parse K.plainChar "" [nonEscapee] === Right nonEscapee

prop_plainChar_unmatched :: Property
prop_plainChar_unmatched = property $ do
  escapee <- forAll $ Gen.element K.escapees
  parse K.plainChar "" [escapee] ?== isLeft

prop_escapedChar_matched :: Property
prop_escapedChar_matched = property $ do
  nonEscapee  <- forAll $ (`elem` K.escapeeChars) `Gen.filter` Gen.ascii
  text        <- forAll $ pure ['\\', nonEscapee]
  parse K.escapedChar "" text === Right (read ['\'', '\\', nonEscapee, '\''])

prop_escapedChar_unmatched_0 :: Property
prop_escapedChar_unmatched_0 = property $ do
  c <- forAll Gen.ascii
  parse K.escapedChar "" [c] ?== isLeft

prop_escapedChar_unmatched_1 :: Property
prop_escapedChar_unmatched_1 = property $ do
  nonEscapee  <- forAll $ (`notElem` K.escapeeChars) `Gen.filter` Gen.ascii
  text        <- forAll $ pure ['\\', nonEscapee]
  parse K.escapedChar "" text ?== isLeft

prop_litBool_matched :: Property
prop_litBool_matched = property $ do
  text <- forAll $ Gen.element ["true", "false"]
  parse K.litBool "" text === Right (text == "true")

prop_litBool_unmatched :: Property
prop_litBool_unmatched = property $ do
  text <- forAll $ (`notElem` ["true", "false"]) `Gen.filter` Gen.string (Range.linear 4 5) Gen.alpha
  parse K.litBool "" text ?== isLeft

prop_brackets_matched :: Property
prop_brackets_matched = property $ do
  pad1 <- forAll $ Gen.string (Range.linear 0 2) (pure ' ')
  pad2 <- forAll $ Gen.string (Range.linear 0 2) (pure ' ')
  pad3 <- forAll $ Gen.string (Range.linear 0 2) (pure ' ')
  text <- forAll $ pure $ "[" ++ pad1 ++ "true" ++ pad2 ++ "]" ++ pad3
  parse (K.brackets K.litBool) "" text === Right True

prop_brackets_unmatched :: Property
prop_brackets_unmatched = property $ do
  pad1 <- forAll $ Gen.string (Range.linear 0 2) (pure ' ')
  pad2 <- forAll $ Gen.string (Range.linear 0 2) (pure ' ')
  text <- forAll $ pure $ "[" ++ pad1 ++ "true" ++ pad2
  parse (K.brackets K.litBool) "" text ?== isLeft

prop_braces_matched :: Property
prop_braces_matched = property $ do
  pad1 <- forAll $ Gen.string (Range.linear 0 2) (pure ' ')
  pad2 <- forAll $ Gen.string (Range.linear 0 2) (pure ' ')
  pad3 <- forAll $ Gen.string (Range.linear 0 2) (pure ' ')
  text <- forAll $ pure $ "{" ++ pad1 ++ "true" ++ pad2 ++ "}" ++ pad3
  parse (K.braces K.litBool) "" text === Right True

prop_braces_unmatched :: Property
prop_braces_unmatched = property $ do
  pad1 <- forAll $ Gen.string (Range.linear 0 2) (pure ' ')
  pad2 <- forAll $ Gen.string (Range.linear 0 2) (pure ' ')
  text <- forAll $ pure $ "{" ++ pad1 ++ "true" ++ pad2
  parse (K.braces K.litBool) "" text ?== isLeft

prop_litString :: Property
prop_litString = property $ error "TODO Implement prop_litString"

prop_array :: Property
prop_array = property $ error "TODO Implement prop_array"

prop_field :: Property
prop_field = property $ error "TODO Implement prop_field"

prop_object :: Property
prop_object = property $ error "TODO Implement prop_object"

prop_nullKeyword :: Property
prop_nullKeyword = property $ error "TODO Implement prop_nullKeyword"

prop_number :: Property
prop_number = property $ error "TODO Implement prop_number"

prop_json :: Property
prop_json = property $ do
  j <- forAll   genJson
  s <- forAll $ pure (toString j)
  parse K.json "" s === Right j

tests :: IO Bool
tests = checkSequential $ reversed $$discover
