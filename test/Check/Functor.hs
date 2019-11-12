{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module Check.Functor where

import Hedgehog
import Hedgehog.Extra
import Koan.Functor   as K
import Prelude        as P

import qualified Data.Functor   as P
import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

instance K.Functor Maybe where
  fmap f (Just a) = Just (f a)
  fmap _ Nothing  = Nothing

prop_fmap :: Property
prop_fmap = property $ do
  ma <- forAll $ G.maybe (G.int R.constantBounded)
  ((+1) `K.fmap` ma) === ((+1) `P.fmap` ma)

prop_fmap_op :: Property
prop_fmap_op = property $ do
  ma <- forAll $ G.maybe (G.int R.constantBounded)
  ((+1) K.<$> ma) === ((+1) P.<$> ma)

prop_fmap_left_op :: Property
prop_fmap_left_op = property $ do
  a  <- forAll $ G.maybe G.alpha
  mb <- forAll $ G.maybe (G.int R.constantBounded)
  (a K.<$ mb) === (a P.<$ mb)

prop_fmap_right_op :: Property
prop_fmap_right_op = property $ do
  a  <- forAll $ G.maybe G.alpha
  mb <- forAll $ G.maybe (G.int R.constantBounded)
  (mb K.$> a) === (mb P.$> a)

prop_fmap_void :: Property
prop_fmap_void = property $ do
  mb <- forAll $ G.maybe (G.int R.constantBounded)
  K.void mb === P.void mb

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
