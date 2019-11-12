{-# LANGUAGE TemplateHaskell #-}

module Check.Alternative where

import Hedgehog
import Hedgehog.Extra

import qualified Hedgehog.Gen     as G
import qualified Hedgehog.Range   as R
import qualified Koan.Applicative as K

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

prop_simple :: Property
prop_simple = property $ do
  True === True

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
