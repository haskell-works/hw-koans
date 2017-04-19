{-# LANGUAGE TemplateHaskell #-}

module Koan.Start where

import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

meaningOfLife :: Int
meaningOfLife = 42

prop_meaningOfLife :: Property
prop_meaningOfLife = property $ meaningOfLife === 42

tests :: IO Bool
tests = ($$(checkConcurrent))
