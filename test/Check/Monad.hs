{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Check.Monad where

import qualified Data.List           as P
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import qualified Prelude             as P

import           Control.Monad
import           Control.Monad.State
import           Data.Maybe
import           Hedgehog
import           Hedgehog.Extra
import           Koan.Monad          as K
import           Prelude             hiding (elem, filter)

instance K.Monad (State Int) where
  (>>=) = (P.>>=)

prop_sequence_op :: Property
prop_sequence_op = property $ do
  i  <- forAll $ Gen.int Range.constantBounded
  a  <- forAll $ Gen.int Range.constantBounded
  b  <- forAll $ Gen.int Range.constantBounded

  runState (modify (+a) K.>> modify (*b)) i === ((), (i + a) * b)

prop_return :: Property
prop_return = property $ do
  i  <- forAll $ Gen.int Range.constantBounded
  a  <- forAll $ Gen.int Range.constantBounded

  runState (K.return a) i === (a, i)

prop_reverseBind :: Property
prop_reverseBind = property $ do
  i  <- forAll $ Gen.int Range.constantBounded
  a  <- forAll $ Gen.int Range.constantBounded
  b  <- forAll $ Gen.int Range.constantBounded

  runState ((\a' -> K.return (a' + b)) K.=<< K.return a) i === (a + b, i)

prop_kleisli_op :: Property
prop_kleisli_op = property $ do
  i  <- forAll $ Gen.int Range.constantBounded
  a  <- forAll $ Gen.int Range.constantBounded
  b  <- forAll $ Gen.int Range.constantBounded
  c  <- forAll $ Gen.int Range.constantBounded

  runState (((\x -> K.return (x + b)) K.>=> (\x -> K.return (x * c))) a) i === ((a + b) * c, i)

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
