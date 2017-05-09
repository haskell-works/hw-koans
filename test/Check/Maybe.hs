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

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
