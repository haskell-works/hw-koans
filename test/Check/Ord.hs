{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Check.Ord where

import Hedgehog
import Hedgehog.Extra
import Koan.Ord       as K
import Prelude        hiding (max, maximum, min, minimum)

import qualified Data.List      as P
import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R
import qualified Prelude        as P

prop_max :: Property
prop_max = property $ do
  a <- forAll $ G.int R.constantBounded
  b <- forAll $ G.int R.constantBounded
  c <- forAll $ G.int R.constantBounded
  a `max` (b `max` c) === (a `max` b) `max` c
  a `max` minBound === a
  minBound `max` a === a
  a `max` maxBound === maxBound
  maxBound `max` a === maxBound

prop_min :: Property
prop_min = property $ do
  a <- forAll $ G.int R.constantBounded
  b <- forAll $ G.int R.constantBounded
  c <- forAll $ G.int R.constantBounded
  a `min` (b `min` c) === (a `min` b) `min` c
  a `min` minBound === minBound
  minBound `min` a === minBound
  a `min` maxBound === a
  maxBound `min` a === a

prop_maximum :: Property
prop_maximum = property $ do
  as <- forAll $ G.list (R.linear 1 100) (G.int R.constantBounded)
  (maximum as `elem` as) === True
  all ((maximum as) >=) as === True

prop_mininimum :: Property
prop_mininimum = property $ do
  as <- forAll $ G.list (R.linear 1 100) (G.int R.constantBounded)
  (minimum as `elem` as) === True
  all ((minimum as) <=) as === True

prop_insert :: Property
prop_insert = property $ do
  a   <- forAll $ G.int R.constantBounded
  as  <- forAll $ G.list (R.linear 1 100) (G.int R.constantBounded)
  K.insert a as === P.insert a as

prop_sort :: Property
prop_sort = property $ do
  as <- forAll $ G.list (R.linear 1 100) (G.int R.constantBounded)
  K.sort as === P.sort as

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
