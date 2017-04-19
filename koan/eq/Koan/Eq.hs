{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Koan.Eq where

import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import           Prelude        (Bool, IO, Int, reverse, ($), (/=), (==))

filterInt :: (Int -> Bool) -> [Int] -> [Int]
filterInt p (x:xs) = if p x then x:filterInt p xs else filterInt p xs
filterInt _ []     = []

prop_filterInt :: Property
prop_filterInt =
  property $ do
    x <- forAll $ Gen.int (Range.constantBounded)
    xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.constantBounded))
    let ys = x:xs
    filterInt (/= x) (x:xs) === filterInt (/= x) xs

tests :: IO Bool
tests = ($$(checkConcurrent))
