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

import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range
import qualified Koan.Parser.Json     as K

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

enrolled :: Bool
enrolled = False

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
  error "TODO: Implement prop_comma_matched"

prop_comma_unmatched :: Property
prop_comma_unmatched = property $ do
  error "TODO: Implement prop_comma_unmatched"

prop_plainChar_matched :: Property
prop_plainChar_matched = property $ do
  error "TODO: Implement prop_plainChar_matched"

prop_plainChar_unmatched :: Property
prop_plainChar_unmatched = property $ do
  error "TODO: Implement prop_plainChar_unmatched"

prop_escapedChar_matched :: Property
prop_escapedChar_matched = property $ do
  error "TODO: Implement prop_escapedChar_matched"

prop_escapedChar_unmatched_0 :: Property
prop_escapedChar_unmatched_0 = property $ do
  error "TODO: Implement prop_escapedChar_unmatched_0"

prop_escapedChar_unmatched_1 :: Property
prop_escapedChar_unmatched_1 = property $ do
  error "TODO: Implement prop_escapedChar_unmatched_1"

prop_litBool_matched :: Property
prop_litBool_matched = property $ do
  error "TODO: Implement prop_litBool_matched"

prop_litBool_unmatched :: Property
prop_litBool_unmatched = property $ do
  error "TODO: Implement prop_litBool_unmatched"

prop_brackets_matched :: Property
prop_brackets_matched = property $ do
  error "TODO: Implement prop_brackets_matched"

prop_brackets_unmatched :: Property
prop_brackets_unmatched = property $ do
  error "TODO: Implement prop_brackets_unmatched"

prop_braces_matched :: Property
prop_braces_matched = property $ do
  error "TODO: Implement prop_braces_matched"

prop_braces_unmatched :: Property
prop_braces_unmatched = property $ do
  error "TODO: Implement prop_braces_unmatched"

prop_litString :: Property
prop_litString = property $ do
  error "TODO Implement prop_litString"

prop_array :: Property
prop_array = property $ do
  error "TODO Implement prop_array"

prop_field :: Property
prop_field = property $ do
  error "TODO Implement prop_field"

prop_object :: Property
prop_object = property $ do
  error "TODO Implement prop_object"

prop_nullKeyword :: Property
prop_nullKeyword = property $ do
  error "TODO Implement prop_nullKeyword"

prop_number :: Property
prop_number = property $ do
  error "TODO Implement prop_number"

prop_json :: Property
prop_json = property $ do
  error "TODO Implement prop_json"

tests :: IO Bool
tests = checkSequential $ reversed $$discover
