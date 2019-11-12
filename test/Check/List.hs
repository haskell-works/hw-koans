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

prop_length :: Property
prop_length = property $ do
  xs  <- forAll $ G.list (R.linear 1 100) (G.int R.constantBounded)
  K.length xs === P.length xs

prop_sumInt :: Property
prop_sumInt = property $ do
  xs  <- forAll $ G.list (R.linear 1 100) (G.int R.constantBounded)
  K.sumInt xs === P.sum xs

prop_productInt :: Property
prop_productInt = property $ do
  xs  <- forAll $ G.list (R.linear 1 100) (G.int R.constantBounded)
  K.productInt xs === P.product xs

prop_sum :: Property
prop_sum = property $ do
  xs  <- forAll $ G.list (R.linear 1 100) (G.int R.constantBounded)
  K.sum xs === P.sum xs

prop_product :: Property
prop_product = property $ do
  xs  <- forAll $ G.list (R.linear 1 100) (G.int R.constantBounded)
  K.product xs === P.product xs

prop_reverse :: Property
prop_reverse = property $ do
  xs  <- forAll $ G.list (R.linear 0 100) (G.int R.constantBounded)
  K.reverse xs === P.reverse xs

prop_intersperse :: Property
prop_intersperse = property $ do
  x   <- forAll $ G.int R.constantBounded
  xs  <- forAll $ G.list (R.linear 0 100) (G.int R.constantBounded)
  K.intersperse x xs === P.intersperse x xs

prop_concat_op :: Property
prop_concat_op = property $ do
  xs  <- forAll $ G.list (R.linear 0 100) (G.int R.constantBounded)
  ys  <- forAll $ G.list (R.linear 0 100) (G.int R.constantBounded)
  xs K.++ ys === xs P.++ ys

prop_concat :: Property
prop_concat = property $ do
  xss  <- forAll $ G.list (R.linear 0 10) $ G.list (R.linear 0 10) $ G.int R.constantBounded
  K.concat xss === P.concat xss

prop_tails :: Property
prop_tails = property $ do
  xs  <- forAll $ G.list (R.linear 0 100) (G.int R.constantBounded)
  K.tails xs === P.tails xs

prop_intercalate :: Property
prop_intercalate = property $ do
  x   <- forAll $ G.list (R.linear 0 100) (G.int R.constantBounded)
  xss <- forAll $ G.list (R.linear 0 100) (G.list (R.linear 0 100) (G.int R.constantBounded))
  K.intercalate x xss === P.intercalate x xss

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

prop_transpose_op :: Property
prop_transpose_op = property $ do
  xss <- forAll $ G.list (R.linear 0 10) $ G.list (R.linear 0 10) $ G.int R.constantBounded
  K.transpose xss === P.transpose xss

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
