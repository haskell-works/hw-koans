{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Check.List where

import qualified Data.List      as P
import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import           Koan.List      as K
import           Prelude        hiding (elem, filter)
import qualified Prelude        as P

prop_head :: Property
prop_head = property $ do
  x   <- forAll $ Gen.int (Range.constantBounded)
  xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  K.head (x:xs) === P.head (x:xs)

prop_tail :: Property
prop_tail = property $ do
  x   <- forAll $ Gen.int (Range.constantBounded)
  xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  K.tail (x:xs) === P.tail (x:xs)

prop_last :: Property
prop_last = property $ do
  x   <- forAll $ Gen.int (Range.constantBounded)
  xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  K.tail (x:xs) === P.tail (x:xs)

prop_reverse :: Property
prop_reverse = property $ do
  xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  K.reverse xs === P.reverse xs

prop_concat_op :: Property
prop_concat_op = property $ do
  xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  ys  <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  xs K.++ ys === xs P.++ ys

prop_concat :: Property
prop_concat = property $ do
  xs  <- forAll $ Gen.list (Range.linear 0 10) $ Gen.list (Range.linear 0 10) $ Gen.int (Range.constantBounded)
  K.concat xs === P.concat xs

prop_tails :: Property
prop_tails = property $ do
  xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  K.tails xs === P.tails xs

tests :: IO Bool
tests = ($$(checkConcurrent))
