{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Check.Maybe where

import qualified Data.List           as P
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import qualified Prelude             as P

import           Control.Applicative
import           Data.Maybe
import           Hedgehog
import           Hedgehog.Extra
import           Koan.Maybe          as K
import           Prelude             hiding (elem, filter)

{-# ANN module ("HLint: Reduce duplication" :: String) #-}

genMaybe :: Monad m => Gen m a -> Gen m (K.Maybe a)
genMaybe g = do
    inJust <- Gen.bool
    if inJust
      then K.Just <$> g
      else pure K.Nothing

enk :: P.Maybe a -> K.Maybe a
enk (P.Just a) = K.Just a
enk P.Nothing  = K.Nothing

unk :: K.Maybe a -> P.Maybe a
unk (K.Just a) = P.Just a
unk K.Nothing  = P.Nothing

prop_orElse :: Property
prop_orElse = property $ do
  ma  <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  b   <- forAll $ Gen.int Range.constantBounded
  enk ma `K.orElse` b === fromMaybe b ma

prop_orMaybe :: Property
prop_orMaybe = property $ do
  ma  <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  mb  <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  unk (enk ma `K.orMaybe` enk mb) === (ma <|> mb)

prop_mapMaybe :: Property
prop_mapMaybe = property $ do
  mb  <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  unk ((+1) `K.mapMaybe` enk mb) === ((+1) <$> mb)

prop_concatMaybes :: Property
prop_concatMaybes = property $ do
  ma  <- forAll $ Gen.list (Range.linear 1 100) (Gen.maybe (Gen.int Range.constantBounded))
  K.concatMaybes (enk <$> ma) === catMaybes ma

prop_filterMaybe :: Property
prop_filterMaybe = property $ do
  ma  <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  unk (K.filterMaybe even (enk ma)) === listToMaybe (P.filter even (catMaybes [ma]))

prop_foldMaybe :: Property
prop_foldMaybe = property $ do
  ma  <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  K.foldMaybe (+) 0 (enk ma) === P.foldr (+) 0 ma

prop_applyMaybe :: Property
prop_applyMaybe = property $ do
  let mf1 = P.Nothing :: P.Maybe (Int -> Int)
  let mf2 = P.Just (+1)
  ma  <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  unk (K.applyMaybe (enk mf1) (enk ma)) === (mf1 P.<*> ma)
  unk (K.applyMaybe (enk mf2) (enk ma)) === (mf2 P.<*> ma)

prop_bindMaybe :: Property
prop_bindMaybe = property $ do
  let mf1 = const P.Nothing :: Int -> P.Maybe Int
  let mf2 = const (P.Just 1) :: Int -> P.Maybe Int
  ma <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  unk (K.bindMaybe (enk <$> mf1) (enk ma)) === (ma P.>>= mf1)
  unk (K.bindMaybe (enk <$> mf2) (enk ma)) === (ma P.>>= mf2)

prop_functor_fmapMaybe :: Property
prop_functor_fmapMaybe = property $ do
  mb  <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  unk ((+1) <$> enk mb) === ((+1) <$> mb)

prop_applicative_pureMaybe :: Property
prop_applicative_pureMaybe = property $ do
  a  <- forAll $ Gen.int Range.constantBounded
  unk (pure a) === pure a

prop_applicative_applyMaybe :: Property
prop_applicative_applyMaybe = property $ do
  mf  <- forAll $ Gen.maybe (pure "(+1)")
  ma  <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  unk ((const (+1) <$> enk mf) <*> enk ma) === ((const (+1) <$> mf) <*> ma)

prop_monad_bind :: Property
prop_monad_bind = property $ do
  mf  <- forAll $ Gen.maybe (pure "(+1)")
  ma  <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  unk ((const (+1) <$> enk mf) <*> enk ma) === ((const (+1) <$> mf) <*> ma)

prop_computeSumInDo :: Property
prop_computeSumInDo = property $ do
  ma  <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  mb  <- forAll $ Gen.maybe (Gen.int Range.constantBounded)
  unk (computeSumInDo (enk ma) (enk mb)) === ((+) <$> ma <*> mb)

prop_mkEndPoint :: Property
prop_mkEndPoint = property $ do
  s <- forAll $ genMaybe $ Gen.string (Range.linear 0 100) Gen.alpha
  p <- forAll $ genMaybe $ Gen.int (Range.linear 0 100)
  unk (K.mkEndPoint s p) === (K.EndPoint <$> unk s <*> unk p)

prop_mkConnection :: Property
prop_mkConnection = property $ do
  sHost <- forAll $ genMaybe $ Gen.string (Range.linear 0 100) Gen.alpha
  sPort <- forAll $ genMaybe $ Gen.int (Range.linear 0 100)
  dHost <- forAll $ genMaybe $ Gen.string (Range.linear 0 100) Gen.alpha
  dPort <- forAll $ genMaybe $ Gen.int (Range.linear 0 100)
  unk (K.mkConnection sHost sPort dHost dPort) ===
    (   K.Connection
    <$> (K.EndPoint <$> unk sHost <*> unk sPort)
    <*> (K.EndPoint <$> unk dHost <*> unk dPort)
    )

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
