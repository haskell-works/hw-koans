{-# LANGUAGE TemplateHaskell #-}

module Check.Start where

import           Hedgehog
import           Hedgehog.Extra
import           Koan.Start

prop_meaningOfLife :: Property
prop_meaningOfLife = property $ meaningOfLife === 42

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
