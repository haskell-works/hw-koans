{-# LANGUAGE TemplateHaskell #-}

module Check.Applicative where

import qualified Koan.Applicative as K

import           Hedgehog
import           Hedgehog.Extra
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

instance K.Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  _ <*> Nothing = Nothing
  (Just f) <*> (Just x) = Just (f x)

prop_apply :: Property
prop_apply = property $ do
  ma <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  (K.pure id K.<*> ma) === ma

prop_apply_return_left :: Property
prop_apply_return_left = property $ do
  ma <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  (K.pure id K.<*> ma) === ma
  mb <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  (ma K.<* mb) === (ma <* mb)

prop_apply_return_right :: Property
prop_apply_return_right = property $ do
  ma <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  mb <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  (ma K.*> mb) === (ma *> mb)

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
