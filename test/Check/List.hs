{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Check.List where

import Hedgehog
import Hedgehog.Extra
import Koan.Applicative as K
import Koan.Functor     as K
import Koan.List        as K
import Koan.Monad       as K
import Prelude          hiding (elem)

import qualified Data.List      as P
import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R
import qualified Prelude        as P

prop_head :: Property
prop_head = property $ do
  xs  <- forAll $ G.list (R.linear 1 100) (G.int R.constantBounded)
  K.head xs === P.head xs

prop_tail :: Property
prop_tail = property $ do
  xs  <- forAll $ G.list (R.linear 1 100) (G.int R.constantBounded)
  K.tail xs === P.tail xs

prop_last :: Property
prop_last = property $ do
  xs  <- forAll $ G.list (R.linear 1 100) (G.int R.constantBounded)
  K.last xs === P.last xs

prop_init :: Property
prop_init = property $ do
  xs  <- forAll $ G.list (R.linear 1 100) (G.int R.constantBounded)
  K.init xs === P.init xs

prop_reverse :: Property
prop_reverse = property $ do
  xs  <- forAll $ G.list (R.linear 0 100) (G.int R.constantBounded)
  K.reverse xs === P.reverse xs

prop_concat_op :: Property
prop_concat_op = property $ do
  xs  <- forAll $ G.list (R.linear 0 100) (G.int R.constantBounded)
  ys  <- forAll $ G.list (R.linear 0 100) (G.int R.constantBounded)
  xs K.++ ys === xs P.++ ys

prop_concat :: Property
prop_concat = property $ do
  xs  <- forAll $ G.list (R.linear 0 10) $ G.list (R.linear 0 10) $ G.int R.constantBounded
  K.concat xs === P.concat xs

prop_tails :: Property
prop_tails = property $ do
  xs  <- forAll $ G.list (R.linear 0 100) (G.int R.constantBounded)
  K.tails xs === P.tails xs

prop_mapList :: Property
prop_mapList = property $ do
  xs <- forAll $ G.list (R.linear 0 100) $ G.int R.constantBounded
  K.mapList (*2) xs === P.fmap (*2) xs

prop_filterList :: Property
prop_filterList = property $ do
  xs <- forAll $ G.list (R.linear 0 100) $ G.int R.constantBounded
  filter even xs === K.filterList even xs

prop_foldlList :: Property
prop_foldlList = property $ do
  xs <- forAll $ G.list (R.linear 0 100) $ G.int R.constantBounded
  K.foldlList (+) 0 xs === foldl (+) 0 xs

prop_foldrList :: Property
prop_foldrList = property $ do
  xs <- forAll $ G.list (R.linear 0 100) $ G.int R.constantBounded
  K.foldrList (+) 0 xs === foldr (+) 0 xs

prop_applyList :: Property
prop_applyList = property $ do
  xs <- forAll $ G.list (R.linear 0 100) $ G.int R.constantBounded
  let fns = [(+ 2), (* 4), \x -> -x, (`mod` 2)]
  K.applyList fns xs === (fns P.<*> xs)

prop_bindList :: Property
prop_bindList = property $ do
  xs <- forAll $ G.list (R.linear 0 100) $ G.int R.constantBounded
  (replicate 5 `bindList` xs) === (xs P.>>= replicate 5)

prop_bind_op :: Property
prop_bind_op = property $ do
  xs <- forAll $ G.list (R.linear 0 100) $ G.int R.constantBounded
  (xs K.>>= replicate 5) === (xs P.>>= replicate 5)

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
