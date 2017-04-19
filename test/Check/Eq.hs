{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Check.Eq where

import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import           Koan.Eq
import           Prelude        hiding (elem, filter)

prop_filterInt :: Property
prop_filterInt =
  property $ do
    x   <- forAll $ Gen.int (Range.constantBounded)
    xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
    length (filterInt (== x) xs) + length (filterInt (/= x) xs) === length xs

prop_filterChar :: Property
prop_filterChar = property $ do
  x   <- forAll $ Gen.enum 'a' 'z'
  xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.enum 'a' 'z')
  length (filterChar (== x) xs) + length (filterChar (/= x) xs) === length xs

prop_filter :: Property
prop_filter = property $ do
  x   <- forAll $ Gen.enum 'a' 'z'
  xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.enum 'a' 'z')
  length (filter (== x) xs) + length (filter (/= x) xs) === length xs

prop_elemInt :: Property
prop_elemInt = property $ do
  x   <- forAll $ Gen.int (Range.constantBounded)
  xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
  x `elemInt` (filter (/= x) xs) === False
  x `elem` (x:xs) === True

prop_elem :: Property
prop_elem = property $ do
  x   <- forAll $ Gen.enum 'a' 'z'
  xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.enum 'a' 'z')
  x `elem` (filter (/= x) xs) === False
  x `elem` (x:xs) === True

tests :: IO Bool
tests = ($$(checkConcurrent))
