{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Koan.Eq where

import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import           Prelude        hiding (elem, filter)

-- Introduction to generics

filterInt :: (Int -> Bool) -> [Int] -> [Int]
filterInt p (x:xs) = if p x then x:filterInt p xs else filterInt p xs
filterInt _ []     = []

prop_filterInt :: Property
prop_filterInt =
  property $ do
    x   <- forAll $ Gen.int (Range.constantBounded)
    xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
    length (filterInt (== x) xs) + length (filterInt (/= x) xs) === length xs

filterChar :: (Char -> Bool) -> [Char] -> [Char]
filterChar p (x:xs) = if p x then x:filterChar p xs else filterChar p xs
filterChar _ []     = []

filter :: (a -> Bool) -> [a] -> [a]
filter p (x:xs) = if p x then x:filter p xs else filter p xs
filter _ []     = []

prop_filterChar :: Property
prop_filterChar =
  property $ do
    x   <- forAll $ Gen.enum 'a' 'z'
    xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.enum 'a' 'z')
    length (filterChar (== x) xs) + length (filterChar (/= x) xs) === length xs

prop_filter :: Property
prop_filter =
  property $ do
    x   <- forAll $ Gen.enum 'a' 'z'
    xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.enum 'a' 'z')
    length (filter (== x) xs) + length (filter (/= x) xs) === length xs

-- Using the Eq typeclass

elemInt :: Int -> [Int] -> Bool
elemInt i (x:xs) = if i == x then True else elemInt i xs
elemInt _ []     = False

elem :: Eq a => a -> [a] -> Bool
elem i (x:xs) = if i == x then True else elem i xs
elem _ []     = False

prop_elemInt :: Property
prop_elemInt =
  property $ do
    x   <- forAll $ Gen.int (Range.constantBounded)
    xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
    x `elemInt` (filter (/= x) xs) === False
    x `elem` (x:xs) === True

prop_elem :: Property
prop_elem =
  property $ do
    x   <- forAll $ Gen.enum 'a' 'z'
    xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.enum 'a' 'z')
    x `elem` (filter (/= x) xs) === False
    x `elem` (x:xs) === True

tests :: IO Bool
tests = ($$(checkConcurrent))
