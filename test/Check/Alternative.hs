{-# LANGUAGE TemplateHaskell #-}

module Check.Alternative where

import qualified Koan.Applicative as K

import           Hedgehog
import           Hedgehog.Extra
import qualified Hedgehog.Gen     as Gen
import qualified Hedgehog.Range   as Range

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

prop_simple :: Property
prop_simple = property $ do
  True === True

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
