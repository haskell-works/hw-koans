{-# LANGUAGE TemplateHaskell #-}

module Check.Alternative where

import Hedgehog
import Hedgehog.Extra

import qualified Hedgehog.Gen     as G
import qualified Hedgehog.Range   as R
import qualified Koan.Applicative as K

{- HLINT ignore "Redundant do"        -}

prop_simple :: Property
prop_simple = property $ do
  True === True

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
