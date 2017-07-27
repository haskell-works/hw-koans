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

{-

Relevant documentation:

* https://hackage.haskell.org/package/hedgehog-0.5/docs/Hedgehog.html
* https://hackage.haskell.org/package/hedgehog-0.5/docs/Hedgehog-Gen.html
* https://hackage.haskell.org/package/hedgehog-0.5/docs/Hedgehog-Range.html

Useful functions:

* Gen.bool
* Gen.string
* Range.constant
* Gen.alpha
* Gen.list
* Range.linear
* Gen.double
* Range.linearFrac
* pure
* forAll
* Gen.filter
* intercalate
* Gen.ascii
* Gen.element
* (===)
* (?==)
* (/==)
* elem
* notElem
* isLeft

-}

enrolled :: Bool
enrolled = False

-- Write a generator that generates Bool values
genBool :: MonadGen m => m Bool
genBool = error "TODO Implement genBool"

-- Write a generator that generates String values
genString :: MonadGen m => m String
genString = error "TODO Implement genString"

-- Write a generator that generates Double values
genNumber :: MonadGen m => m Double
genNumber = error "TODO Implement genNumber"

-- Write a generator that generates JSON object fields
genField :: MonadGen m => Int -> m (String, K.Json)
genField = error "TODO Implement genField"

-- Write a generator that generates list of tuples that represents a Json
-- object
genObject :: MonadGen m => Int -> m [(String, K.Json)]
genObject = error "TODO Implement genObject"

-- Write a generator that generates list of Json values
genArray :: MonadGen m => Int -> m [K.Json]
genArray = error "TODO Implement genArray"

-- Write a generator that generates arbitrary JSON
genJson :: MonadGen m => m K.Json
genJson = error "TODO Implement genJson"

-- Write a function that converts a Json to a String
toString :: K.Json -> String
toString = error "TODO Implement toString"

-- Write a property that asserts parser successfully parses valid comma
prop_comma_matched :: Property
prop_comma_matched = property $ do
  error "TODO: Implement prop_comma_matched"

-- Write a property that asserts parser fails to parse invalid comma
prop_comma_unmatched :: Property
prop_comma_unmatched = property $ do
  error "TODO: Implement prop_comma_unmatched"

-- Write a property that asserts parser successfully parses valid plainChar
-- a plainChar is a character that is not escaped.
prop_plainChar_matched :: Property
prop_plainChar_matched = property $ do
  error "TODO: Implement prop_plainChar_matched"

-- Write a property that asserts parser fails to parse invalid plainChar
-- a plainChar is a character that is not escaped.
prop_plainChar_unmatched :: Property
prop_plainChar_unmatched = property $ do
  error "TODO: Implement prop_plainChar_unmatched"

-- Write a property that asserts parser successfully parses valid escapedChar
-- a escapedChar is one of these: \\ \" \n \r \t
prop_escapedChar_matched :: Property
prop_escapedChar_matched = property $ do
  error "TODO: Implement prop_escapedChar_matched"

-- Write a property that asserts parser fails to parse single character
prop_escapedChar_unmatched_0 :: Property
prop_escapedChar_unmatched_0 = property $ do
  error "TODO: Implement prop_escapedChar_unmatched_0"

-- Write a property that asserts parser fails to parse invalid escaped character
-- i.e. an attempt to escape a character that is not an escapedChar.
prop_escapedChar_unmatched_1 :: Property
prop_escapedChar_unmatched_1 = property $ do
  error "TODO: Implement prop_escapedChar_unmatched_1"

-- Write a property that asserts parser successfully parses null
prop_nullKeyword :: Property
prop_nullKeyword = property $ do
  error "TODO Implement prop_nullKeyword"

-- Write a property that asserts parser fails to parse a sequence of alpha
-- characters that is not null
prop_nullKeyword_unmatched :: Property
prop_nullKeyword_unmatched = property $ do
  error "TODO: Implement prop_nullKeyword_unmatched"

-- Write a property that asserts parser successfully parses true or false
prop_litBool_matched :: Property
prop_litBool_matched = property $ do
  error "TODO: Implement prop_litBool_matched"

-- Write a property that asserts parser fails to parse a sequence of alpha
-- characters that is not either true or false
prop_litBool_unmatched :: Property
prop_litBool_unmatched = property $ do
  error "TODO: Implement prop_litBool_unmatched"

-- Write a property that asserts parser successfully parses brackets
prop_brackets_matched :: Property
prop_brackets_matched = property $ do
  error "TODO: Implement prop_brackets_matched"

-- Write a property that asserts parser fails to parse when closing brackets
-- missing
prop_brackets_unmatched :: Property
prop_brackets_unmatched = property $ do
  error "TODO: Implement prop_brackets_unmatched"

-- Write a property that asserts parser successfully parses braces
prop_braces_matched :: Property
prop_braces_matched = property $ do
  error "TODO: Implement prop_braces_matched"

-- Write a property that asserts parser fails to parse when closing braces
-- missing
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

prop_number :: Property
prop_number = property $ do
  error "TODO Implement prop_number"

prop_json :: Property
prop_json = property $ do
  error "TODO Implement prop_json"

tests :: IO Bool
tests = checkSequential $ reversed $$discover
