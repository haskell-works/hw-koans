{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Check.Ord where

import           Control.Monad.Trans
import           Data.Monoid
import           Hedgehog
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Koan.Ord
import           Prelude             hiding (max, maximum, min, minimum)

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

prop_maximum :: Property
prop_maximum = property $ do
  as <- forAll $ Gen.list (Range.linear 1 100) (Gen.int (Range.constantBounded))
  (maximum as `elem` as) === True
  all ((maximum as) >=) as === True

prop_mininimum :: Property
prop_mininimum = property $ do
  as <- forAll $ Gen.list (Range.linear 1 100) (Gen.int (Range.constantBounded))
  (minimum as `elem` as) === True
  all ((minimum as) <=) as === True

tests :: IO Bool
tests = ($$(checkConcurrent))
