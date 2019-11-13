{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Check.Maybe where

import Control.Applicative
import Data.Maybe
import Hedgehog
import Hedgehog.Extra
import Koan.Maybe          as K
import Prelude             hiding (elem, filter)

import qualified Data.List      as P
import qualified Data.Maybe     as P
import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R
import qualified Prelude        as P

{-# ANN module ("HLint: Reduce duplication" :: String) #-}

genMaybe :: MonadGen m => m a -> m (K.Maybe a)
genMaybe g = do
    inJust <- G.bool
    if inJust
      then K.Just <$> g
      else pure K.Nothing

enk :: P.Maybe a -> K.Maybe a
enk (P.Just a) = K.Just a
enk P.Nothing  = K.Nothing

unk :: K.Maybe a -> P.Maybe a
unk (K.Just a) = P.Just a
unk K.Nothing  = P.Nothing

prop_isJust :: Property
prop_isJust = property $ do
  ma  <- forAll $ G.maybe (G.int R.constantBounded)
  K.isJust (enk ma) === P.isJust ma

prop_isNothing :: Property
prop_isNothing = property $ do
  ma  <- forAll $ G.maybe (G.int R.constantBounded)
  K.isNothing (enk ma) === P.isNothing ma

prop_fromMaybe :: Property
prop_fromMaybe = property $ do
  ma  <- forAll $ G.maybe (G.int R.constantBounded)
  b   <- forAll $ G.int R.constantBounded
  K.fromMaybe b (enk ma) === P.fromMaybe b ma

prop_orMaybe :: Property
prop_orMaybe = property $ do
  ma  <- forAll $ G.maybe (G.int R.constantBounded)
  mb  <- forAll $ G.maybe (G.int R.constantBounded)
  unk (enk ma `K.orMaybe` enk mb) === (ma <|> mb)

prop_mapMaybe :: Property
prop_mapMaybe = property $ do
  mb  <- forAll $ G.maybe (G.int R.constantBounded)
  unk ((+1) `K.mapMaybe` enk mb) === ((+1) <$> mb)

prop_maybe :: Property
prop_maybe = property $ do
  mb  <- forAll $ G.maybe (G.int R.constantBounded)
  unk ((+1) `K.mapMaybe` enk mb) === ((+1) <$> mb)

prop_catMaybes :: Property
prop_catMaybes = property $ do
  ma  <- forAll $ G.list (R.linear 1 100) (G.maybe (G.int R.constantBounded))
  K.catMaybes (enk <$> ma) === P.catMaybes ma

prop_filterMaybe :: Property
prop_filterMaybe = property $ do
  ma  <- forAll $ G.maybe (G.int R.constantBounded)
  unk (K.filterMaybe even (enk ma)) === listToMaybe (P.filter even (P.catMaybes [ma]))

prop_foldMaybe :: Property
prop_foldMaybe = property $ do
  ma  <- forAll $ G.maybe (G.int R.constantBounded)
  K.foldMaybe (+) 0 (enk ma) === P.foldr (+) 0 ma

prop_applyMaybe :: Property
prop_applyMaybe = property $ do
  let mf1 = P.Nothing :: P.Maybe (Int -> Int)
  let mf2 = P.Just (+1)
  ma  <- forAll $ G.maybe (G.int R.constantBounded)
  unk (K.applyMaybe (enk mf1) (enk ma)) === (mf1 P.<*> ma)
  unk (K.applyMaybe (enk mf2) (enk ma)) === (mf2 P.<*> ma)

prop_bindMaybe :: Property
prop_bindMaybe = property $ do
  let mf1 = const P.Nothing :: Int -> P.Maybe Int
  let mf2 = const (P.Just 1) :: Int -> P.Maybe Int
  ma <- forAll $ G.maybe (G.int R.constantBounded)
  unk (K.bindMaybe (enk <$> mf1) (enk ma)) === (ma P.>>= mf1)
  unk (K.bindMaybe (enk <$> mf2) (enk ma)) === (ma P.>>= mf2)

prop_functor_fmapMaybe :: Property
prop_functor_fmapMaybe = property $ do
  mb  <- forAll $ G.maybe (G.int R.constantBounded)
  unk ((+1) <$> enk mb) === ((+1) <$> mb)

prop_applicative_pureMaybe :: Property
prop_applicative_pureMaybe = property $ do
  a  <- forAll $ G.int R.constantBounded
  unk (pure a) === pure a

prop_applicative_applyMaybe :: Property
prop_applicative_applyMaybe = property $ do
  mf  <- forAll $ G.maybe (pure "(+1)")
  ma  <- forAll $ G.maybe (G.int R.constantBounded)
  unk ((const (+1) <$> enk mf) <*> enk ma) === ((const (+1) <$> mf) <*> ma)

prop_monad_bind :: Property
prop_monad_bind = property $ do
  mf  <- forAll $ G.maybe (pure "(+1)")
  ma  <- forAll $ G.maybe (G.int R.constantBounded)
  unk ((const (+1) <$> enk mf) <*> enk ma) === ((const (+1) <$> mf) <*> ma)

prop_computeSumInDo :: Property
prop_computeSumInDo = property $ do
  ma  <- forAll $ G.maybe (G.int R.constantBounded)
  mb  <- forAll $ G.maybe (G.int R.constantBounded)
  unk (computeSumInDo (enk ma) (enk mb)) === ((+) <$> ma <*> mb)

prop_mkEndPoint :: Property
prop_mkEndPoint = property $ do
  s <- forAll $ genMaybe $ G.string (R.linear 0 100) G.alpha
  p <- forAll $ genMaybe $ G.int (R.linear 0 100)
  unk (K.mkEndPoint s p) === (K.EndPoint <$> unk s <*> unk p)

prop_mkConnection :: Property
prop_mkConnection = property $ do
  sHost <- forAll $ genMaybe $ G.string (R.linear 0 100) G.alpha
  sPort <- forAll $ genMaybe $ G.int (R.linear 0 100)
  dHost <- forAll $ genMaybe $ G.string (R.linear 0 100) G.alpha
  dPort <- forAll $ genMaybe $ G.int (R.linear 0 100)
  unk (K.mkConnection sHost sPort dHost dPort) ===
    (   K.Connection
    <$> (K.EndPoint <$> unk sHost <*> unk sPort)
    <*> (K.EndPoint <$> unk dHost <*> unk dPort)
    )

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
