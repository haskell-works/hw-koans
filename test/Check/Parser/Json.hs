{-# LANGUAGE TemplateHaskell #-}

module Check.Parser.Json where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Hedgehog
import Hedgehog.Extra
import Text.Megaparsec

import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range
import qualified Koan.Parser.Json     as K

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

genBool :: Monad m => Gen.Gen m Bool
genBool = Gen.bool

genString :: Monad m => Gen.Gen m String
genString = Gen.string (Range.constant 0 8) Gen.alpha

genObject :: Monad m => Int -> Gen.Gen m [(String, K.Json)]
genObject n = Gen.list (Range.linear 0 2) (genField (n - 1))

genArray :: Monad m => Int -> Gen.Gen m [K.Json]
genArray n = Gen.list (Range.linear 0 2) (genJson' (n - 1))

genNumber :: Monad m => Gen.Gen m Double
genNumber = Gen.double (Range.linearFrac (-100.0) 100.0)

genJson :: Monad m => Gen.Gen m K.Json
genJson = genJson' 3

genJson' :: Monad m => Int -> Gen.Gen m K.Json
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

genField :: Monad m => Int -> Gen.Gen m (String, K.Json)
genField n = (,) <$> Gen.string (Range.constant 0 4) Gen.alpha <*> genJson' n

toString2 :: (String, K.Json) -> String
toString2 (field, value) = show field ++ ":" ++ toString value

toString :: K.Json -> String
toString (K.JsonBool    v) = if v then "true" else "false"
toString (K.JsonNumber  v) = show v
toString (K.JsonString  v) = show v
toString (K.JsonArray   v) = "[" ++ intercalate "," (toString  <$> v) ++ "]"
toString (K.JsonObject  v) = "{" ++ intercalate "," (toString2 <$> v) ++ "}"
toString  K.JsonNull       = "null"

prop_brackets :: Property
prop_brackets = property $ error "TODO Implement prop_brackets"

prop_braces :: Property
prop_braces = property $ error "TODO Implement prop_braces"

prop_plainChar :: Property
prop_plainChar = property $ error "TODO Implement prop_plainChar"

prop_escapedChar :: Property
prop_escapedChar = property $ error "TODO Implement prop_escapedChar"

prop_litString :: Property
prop_litString = property $ error "TODO Implement prop_litString"

prop_litBool :: Property
prop_litBool = property $ error "TODO Implement prop_litBool"

prop_comma :: Property
prop_comma = property $ error "TODO Implement prop_comma"

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
