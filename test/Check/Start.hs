{-# LANGUAGE TemplateHaskell #-}

module Check.Start where

import           Hedgehog
import           Koan.Start

prop_meaningOfLife :: Property
prop_meaningOfLife = property $ meaningOfLife === 42

tests :: IO Bool
tests = checkParallel $$(discover)
