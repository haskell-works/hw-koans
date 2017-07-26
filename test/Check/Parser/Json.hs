{-# LANGUAGE TemplateHaskell #-}

module Check.Parser.Json where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Char
import Data.Either
import Data.List
import Hedgehog
import Text.Megaparsec
import Hedgehog.Extra
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
  nonEscapee <- forAll $ (`notElem` K.escapees) `Gen.filter` Gen.alpha
  parse K.plainChar "" [nonEscapee] === Right nonEscapee

prop_plainChar_unmatched :: Property
prop_plainChar_unmatched = property $ do
  escapee <- forAll $ Gen.element K.escapees
  parse K.plainChar "" [escapee] ?== isLeft

prop_brackets :: Property
prop_brackets = property $ error "TODO Implement prop_brackets"

prop_braces :: Property
prop_braces = property $ error "TODO Implement prop_braces"

prop_escapedChar :: Property
prop_escapedChar = property $ error "TODO Implement prop_escapedChar"

prop_litString :: Property
prop_litString = property $ error "TODO Implement prop_litString"

prop_litBool :: Property
prop_litBool = property $ error "TODO Implement prop_litBool"

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
