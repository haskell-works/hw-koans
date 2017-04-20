{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Check.Eq where

import qualified Data.List      as P
import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import           Koan.Eq        as K
import           Prelude        hiding (elem, filter)
import qualified Prelude        as P

prop_filterInt :: Property
prop_filterInt = property $ do
  x   <- forAll $ Gen.int (Range.constantBounded)
  xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  K.filterInt (== x) xs === P.filter (== x) xs

prop_filterChar :: Property
prop_filterChar = property $ do
  x   <- forAll $ Gen.enum 'a' 'z'
  xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.enum 'a' 'z')
  K.filterChar (== x) xs === P.filter (== x) xs

prop_filter :: Property
prop_filter = property $ do
  x   <- forAll $ Gen.enum 'a' 'z'
  xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.enum 'a' 'z')
  K.filter (== x) xs === P.filter (== x) xs

prop_elemInt :: Property
prop_elemInt = property $ do
  x   <- forAll $ Gen.int (Range.constantBounded)
  xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  (x `K.elemInt` xs) === (x `P.elem` xs)

prop_elem :: Property
prop_elem = property $ do
  x   <- forAll $ Gen.enum 'a' 'z'
  xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.enum 'a' 'z')
  (x `K.elem` xs) === (x `P.elem` xs)

prop_nub :: Property
prop_nub = property $ do
  xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.enum 'a' 'z')
  (K.nub xs) === (P.nub xs)

prop_isPrefixOf :: Property
prop_isPrefixOf = property $ do
  xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.enum 'a' 'z')
  ys  <- forAll $ Gen.list (Range.linear 0 100) (Gen.enum 'a' 'z')
  (xs `K.isPrefixOf` ys) === (xs `P.isPrefixOf` ys)

tests :: IO Bool
tests = ($$(checkConcurrent))
