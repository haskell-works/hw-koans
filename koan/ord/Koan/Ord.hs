{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Koan.Ord where

import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import           Prelude        hiding (max, min)

-- Introduction to generics

max :: Ord a => a -> a -> a
max a b = if a > b then a else b

prop_max :: Property
prop_max = property $ do
  a <- forAll $ Gen.int (Range.constantBounded)
  b <- forAll $ Gen.int (Range.constantBounded)
  c <- forAll $ Gen.int (Range.constantBounded)
  a `max` (b `max` c) === (a `max` b) `max` c
  a `max` minBound === a
  minBound `max` a === a
  a `max` maxBound === maxBound
  maxBound `max` a === maxBound

min :: Ord a => a -> a -> a
min a b = if a < b then a else b

prop_min :: Property
prop_min = property $ do
  a <- forAll $ Gen.int (Range.constantBounded)
  b <- forAll $ Gen.int (Range.constantBounded)
  c <- forAll $ Gen.int (Range.constantBounded)
  a `min` (b `min` c) === (a `min` b) `min` c
  a `min` minBound === minBound
  minBound `min` a === minBound
  a `min` maxBound === a
  maxBound `min` a === a

tests :: IO Bool
tests = ($$(checkConcurrent))
