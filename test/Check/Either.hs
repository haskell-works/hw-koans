{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Check.Either where

import Control.Applicative
import Data.Either         as P
import Hedgehog
import Hedgehog.Extra
import Koan.Either         as K
import Prelude             hiding (elem, filter)

import qualified Data.Bifunctor as Bi
import qualified Data.List      as P
import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R
import qualified Prelude        as P

toK :: P.Either a b -> K.Either a b
toK (P.Left a)  = K.Left a
toK (P.Right b) = K.Right b

frK :: K.Either a b -> P.Either a b
frK (K.Left a)  = P.Left a
frK (K.Right b) = P.Right b

genEither :: MonadGen m => m a -> m b -> m (P.Either a b)
genEither genA genB =
  G.sized $ \n ->
    G.frequency [
      (1 + fromIntegral n, P.Right <$> genB),
      (1 + fromIntegral n, P.Left <$> genA)
    ]

genListEither = G.list
                  (R.linear 1 100)
                  (genEither (G.int R.constantBounded) (G.int R.constantBounded))

prop_isLeft :: Property
prop_isLeft = property $ do
  eab <- forAll $ genEither (G.int R.constantBounded) (G.int R.constantBounded)
  K.isLeft (toK eab) === P.isLeft eab

prop_isRight :: Property
prop_isRight = property $ do
  eab <- forAll $ genEither (G.int R.constantBounded) (G.int R.constantBounded)
  K.isRight (toK eab) === P.isRight eab

prop_lefts :: Property
prop_lefts = property $ do
  leab <- forAll genListEither
  K.lefts (toK <$> leab) === P.lefts leab

prop_rights :: Property
prop_rights = property $ do
  leab <- forAll genListEither
  K.rights (toK <$> leab) === P.rights leab

prop_either :: Property
prop_either = property $ do
  eab <- forAll $ genEither (G.int R.constantBounded) (G.int R.constantBounded)
  K.either (*2) (+1) (toK eab) === P.either (*2) (+1) eab

prop_partition :: Property
prop_partition = property $ do
  leab <- forAll genListEither
  K.partition (toK <$> leab) === P.partitionEithers leab

prop_mapEither :: Property
prop_mapEither = property $ do
  eab <- forAll $ genEither (G.int R.constantBounded) (G.int R.constantBounded)
  frK (mapEither (*2) (toK eab)) === ((*2) P.<$> eab)

prop_bimapEither :: Property
prop_bimapEither = property $ do
  eab <- forAll $ genEither (G.int R.constantBounded) (G.int R.constantBounded)
  frK (bimapEither (+3) (*2) (toK eab)) === Bi.bimap (+3) (*2) eab

prop_applyEitherRight :: Property
prop_applyEitherRight = property $ do
  eab <- forAll $ genEither (G.int R.constantBounded) (G.int R.constantBounded)
  frK (applyEither (K.Right (*2)) (toK eab)) === (pure (*2) <*> eab)

prop_applyEitherLeft :: Property
prop_applyEitherLeft = property $ do
  eab <- forAll $ genEither (G.int R.constantBounded) (G.int R.constantBounded)
  i <- forAll $ G.int R.constantBounded
  let l = frK (applyEither (K.Left i) (toK eab)) :: P.Either Int Int
  let r = P.Left i <*> eab
  l === r

prop_bindEither :: Property
prop_bindEither = property $ do
  eabIn <- forAll $ genEither (G.int R.constantBounded) (G.int R.constantBounded)
  eabOut <- forAll $ genEither (G.int R.constantBounded) (G.int R.constantBounded)
  frK (bindEither (const (toK eabOut)) (toK eabIn)) === (eabIn >>= (const eabOut))

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
