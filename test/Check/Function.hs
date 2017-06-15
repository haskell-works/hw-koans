{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module Check.Function where

import Hedgehog
import Hedgehog.Extra
import Koan.Applicative as K
import Koan.Function    as K
import Koan.Functor     as K
import Koan.Monad       as K
import Prelude          as P

import qualified Data.Function  as P
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

prop_compose_op :: Property
prop_compose_op = property $ do
  a <- forAll $ Gen.int Range.constantBounded
  let f = (+1)
  let g = (+2)
  (f K.. g) a === (f P.. g) a

prop_flip :: Property
prop_flip = property $ do
  a <- forAll $ Gen.int Range.constantBounded
  b <- forAll $ Gen.int Range.constantBounded
  K.flip (-) a b === b - a

prop_mapFunction :: Property
prop_mapFunction = property $ do
  a <- forAll $ Gen.int Range.constantBounded
  ((+3) `K.mapFunction` (+2)) a === ((+3) P.<$> (+2)) a

prop_applyFunction :: Property
prop_applyFunction = property $ do
  a <- forAll $ Gen.int Range.constantBounded
  ((*) `K.applyFunction` (+2)) a === ((*) P.<*> (+2)) a

prop_bindFunction :: Property
prop_bindFunction = property $ do
  a <- forAll $ Gen.int Range.constantBounded
  ((*) `K.bindFunction` (+2)) a === ((+2) P.>>= (*)) a

prop_map_op :: Property
prop_map_op = property $ do
  a <- forAll $ Gen.int Range.constantBounded
  ((+3) K.<$> (+2)) a === ((+3) P.<$> (+2)) a

prop_apply_op :: Property
prop_apply_op = property $ do
  a <- forAll $ Gen.int Range.constantBounded
  ((*) K.<*> (+2)) a === ((*) P.<*> (+2)) a

prop_bind_op :: Property
prop_bind_op = property $ do
  a <- forAll $ Gen.int Range.constantBounded
  ((+2) K.>>= (*)) a === ((+2) P.>>= (*)) a

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
