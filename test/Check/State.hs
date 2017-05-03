{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Check.State where

import qualified Control.Monad.State as P
import qualified Koan.State          as K

import           Hedgehog
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range

nativeState :: K.State s a -> P.State s a
nativeState (K.State run) = P.state run

instance Show (K.State s a) where
  show _ = "state"

prop_return :: Property
prop_return = property $ do
  argument <- forAll $ Gen.int (Range.linear 0 100)
  initial <- forAll $ Gen.int (Range.linear 0 100)
  K.runState (return argument) initial === (argument, initial)

prop_fmap :: Property
prop_fmap = property $ do
  argument <- forAll $ Gen.int (Range.linear 0 100)
  initial <- forAll $ Gen.int (Range.linear 0 100)
  K.runState ((+1) <$> return argument) initial === P.runState ((+1) <$> nativeState (return argument)) initial

prop_get :: Property
prop_get = property $ do
  initial <- forAll $ Gen.int (Range.linear 0 100)
  K.runState K.get initial === (initial, initial)

prop_put :: Property
prop_put = property $ do
  initial <- forAll $ Gen.int (Range.linear 0 100)
  value <- forAll $ Gen.int (Range.linear 0 100)
  K.runState (K.put value) initial === ((), value)

prop_modify :: Property
prop_modify = property $ do
  initial <- forAll $ Gen.int (Range.linear 0 100)
  offset <- forAll $ Gen.int (Range.linear 0 100)
  K.runState (K.modify (+offset)) initial === ((), initial + offset)

prop_bind :: Property
prop_bind = property $ do
  initial <- forAll $ Gen.int (Range.linear 0 100)
  value <- forAll $ Gen.int (Range.linear 0 100)
  K.runState (K.put value >> K.get >>= \a -> K.put (a + 1)) initial === ((), value + 1)

tests :: IO Bool
tests = ($$(checkConcurrent))
