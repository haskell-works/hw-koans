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

genJson :: Monad m => Gen.Gen m K.Json
genJson = genJson' 3

genJson' :: Monad m => Int -> Gen.Gen m K.Json
genJson' n | n <= 0 = Gen.choice
  [ pure   K.JsonNull
  , K.JsonBool   <$> Gen.bool
  , K.JsonNumber <$> Gen.double (Range.linearFrac (-100.0) 100.0)
  ]
genJson' n = Gen.choice
  [ pure   K.JsonNull
  , K.JsonBool   <$> Gen.bool
  , K.JsonNumber <$> Gen.double (Range.linearFrac (-100.0) 100.0)
  , K.JsonArray  <$> Gen.list (Range.linear 0 2) (genJson' (n - 1))
  , K.JsonObject <$> Gen.list (Range.linear 0 2) (genField (n - 1))
  , K.JsonString <$> Gen.string (Range.constant 0 8) Gen.alpha
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

prop_json :: Property
prop_json = property $ do
  j <- forAll   genJson
  s <- forAll $ pure (toString j)
  parse K.json "" s === Right j

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
