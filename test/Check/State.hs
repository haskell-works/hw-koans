{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Check.State where

import Hedgehog
import Hedgehog.Extra

import qualified Control.Monad.State as P
import qualified Hedgehog.Gen        as G
import qualified Hedgehog.Range      as R
import qualified Koan.State          as K

nativeState :: K.State s a -> P.State s a
nativeState (K.State run) = P.state run

instance Show (K.State s a) where
  show _ = "state"

prop_return :: Property
prop_return = property $ do
  argument <- forAll $ G.int (R.linear 0 100)
  initial  <- forAll $ G.int (R.linear 0 100)
  K.runState (return argument) initial === (argument, initial)

prop_fmap :: Property
prop_fmap = property $ do
  argument <- forAll $ G.int (R.linear 0 100)
  initial  <- forAll $ G.int (R.linear 0 100)
  K.runState ((+1) <$> return argument) initial === P.runState ((+1) <$> nativeState (return argument)) initial

prop_get :: Property
prop_get = property $ do
  initial <- forAll $ G.int (R.linear 0 100)
  K.runState K.get initial === (initial, initial)

prop_put :: Property
prop_put = property $ do
  initial <- forAll $ G.int (R.linear 0 100)
  value   <- forAll $ G.int (R.linear 0 100)
  K.runState (K.put value) initial === ((), value)

prop_modify :: Property
prop_modify = property $ do
  initial <- forAll $ G.int (R.linear 0 100)
  offset  <- forAll $ G.int (R.linear 0 100)
  K.runState (K.modify (+offset)) initial === ((), initial + offset)

prop_bind :: Property
prop_bind = property $ do
  initial <- forAll $ G.int (R.linear 0 100)
  value   <- forAll $ G.int (R.linear 0 100)
  K.runState (K.put value >> K.get >>= \a -> K.put (a + 1)) initial === ((), value + 1)

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
