{-# LANGUAGE TemplateHaskell #-}

module Check.Applicative where

import qualified Koan.Applicative as K

import           Hedgehog
import qualified Hedgehog.Gen     as Gen
import qualified Hedgehog.Range   as Range

nativeMaybe :: K.Maybe a -> Maybe a
nativeMaybe (K.Just a) = Just a
nativeMaybe K.Nothing  = Nothing

genMaybe :: Monad m => Gen m a -> Gen m (K.Maybe a)
genMaybe g = do
    inJust <- Gen.bool
    if inJust
      then K.Just <$> g
      else pure K.Nothing

prop_just :: Property
prop_just = property $ do
  a <- forAll Gen.alpha
  K.just a === K.Just a

prop_fmapInMaybe :: Property
prop_fmapInMaybe = property $ do
  a <- forAll $ genMaybe $ Gen.int (Range.linear 1 100)
  nativeMaybe (K.fmapInMaybe (+1) a) === ((+1) <$> nativeMaybe a)

prop_applyInMaybe :: Property
prop_applyInMaybe = property $ do
  a <- forAll $ do
    inJust <- Gen.bool
    if inJust
      then K.Just <$> Gen.int (Range.linear 1 100)
      else pure K.Nothing
  nativeMaybe (K.applyInMaybe (K.Just (+1)) a) === (Just (+1) <*> nativeMaybe a)

prop_add3To9InMaybe :: Property
prop_add3To9InMaybe = property $ K.add3To9InMaybe === K.Just 12

prop_mkEndPoint :: Property
prop_mkEndPoint = property $ do
  s <- forAll $ genMaybe $ Gen.string (Range.linear 0 100) Gen.alpha
  p <- forAll $ genMaybe $ Gen.int (Range.linear 0 100)
  nativeMaybe (K.mkEndPoint s p) === (K.EndPoint <$> nativeMaybe s <*> nativeMaybe p)

prop_mkConnection :: Property
prop_mkConnection = property $ do
  sHost <- forAll $ genMaybe $ Gen.string (Range.linear 0 100) Gen.alpha
  sPort <- forAll $ genMaybe $ Gen.int (Range.linear 0 100)
  dHost <- forAll $ genMaybe $ Gen.string (Range.linear 0 100) Gen.alpha
  dPort <- forAll $ genMaybe $ Gen.int (Range.linear 0 100)
  nativeMaybe (K.mkConnection sHost sPort dHost dPort) ===
    (   K.Connection
    <$> (K.EndPoint <$> nativeMaybe sHost <*> nativeMaybe sPort)
    <*> (K.EndPoint <$> nativeMaybe dHost <*> nativeMaybe dPort)
    )

tests :: IO Bool
tests = checkParallel $$(discover)
