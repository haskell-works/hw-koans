{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Check.Reader where

import Hedgehog
import Hedgehog.Extra

import qualified Control.Monad.Reader as R
import qualified Hedgehog.Gen         as G
import qualified Hedgehog.Range       as R
import qualified Koan.Reader          as K

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

nativeReader :: K.Reader r a -> R.Reader r a
nativeReader (K.Reader run) = R.reader run

instance Show (K.Reader r a) where
  show _ = "reader"

prop_return :: Property
prop_return = property $ do
  env <- forAll $ G.int (R.linear 0 100)
  arg <- forAll $ G.int (R.linear 0 100)
  K.runReader (return arg) env === arg

prop_fmap :: Property
prop_fmap = property $ do
  env <- forAll $ G.int (R.linear 0 100)
  arg <- forAll $ G.int (R.linear 0 100)
  K.runReader ((+1) <$> return arg) env === R.runReader ((+1) <$> return arg) env

prop_apply :: Property
prop_apply = property $ do
  env <- forAll $ G.int (R.linear 0 100)
  arg <- forAll $ G.int (R.linear 0 100)
  K.runReader (return (+1) <*> return arg) env === R.runReader (return (+1) <*> return arg) env

prop_ask :: Property
prop_ask = property $ do
  env <- forAll $ G.int (R.linear 0 100)
  K.runReader K.ask env === env
  K.runReader K.ask env === R.runReader R.ask env

prop_asks :: Property
prop_asks = property $ do
  env <- forAll $ G.int (R.linear 0 100)
  K.runReader (K.asks (+1)) env === env + 1
  K.runReader (K.asks (+1)) env === R.runReader (R.asks (+1)) env

prop_bind :: Property
prop_bind = property $ do
  env <- forAll $ G.int (R.linear 0 100)
  K.runReader (K.asks (+1) >>= (\a -> K.asks (+a))) env === 2 * env + 1

--------------------------------------------------------------------------------
prop_url :: Property
prop_url = property $ do
  hst <- forAll $ G.string (R.linear 1 20) G.alphaNum
  pth <- forAll $ G.string (R.linear 1 20) G.alphaNum
  prt <- forAll $ G.int (R.linear 20 65534)
  K.runReader (K.curlCmd pth) (K.Options hst prt) === K.ShellCommand ("curl https://" <> hst <> ":" <> show prt <> "/" <> pth)

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
