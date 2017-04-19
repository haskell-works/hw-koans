{-# LANGUAGE TemplateHaskell #-}

module Koan.Functor where

import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs

tests :: IO Bool
tests = ($$(checkConcurrent))
