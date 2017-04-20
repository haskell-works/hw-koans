{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module Check.Functor where

import qualified Data.Functor   as P
import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import           Koan.Functor   as K
import           Prelude
import qualified Prelude        as P

instance K.Functor Maybe where
  fmap f (Just a) = Just (f a)
  fmap _ Nothing  = Nothing

prop_fmap :: Property
prop_fmap = property $ do
  ma <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  ((+1) `K.fmap` ma) === ((+1) `P.fmap` ma)

prop_fmap_op :: Property
prop_fmap_op = property $ do
  ma <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  ((+1) K.<$> ma) === ((+1) P.<$> ma)

prop_fmap_left_op :: Property
prop_fmap_left_op = property $ do
  a  <- forAll $ Gen.maybe Gen.alpha
  mb <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  (a K.<$ mb) === (a P.<$ mb)

prop_fmap_right_op :: Property
prop_fmap_right_op = property $ do
  a  <- forAll $ Gen.maybe Gen.alpha
  mb <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  (mb K.$> a) === (mb P.$> a)

prop_fmap_void :: Property
prop_fmap_void = property $ do
  mb <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  K.void mb === P.void mb

tests :: IO Bool
tests = ($$(checkConcurrent))
