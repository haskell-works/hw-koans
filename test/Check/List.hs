{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Check.List where

import qualified Data.List        as P
import           Hedgehog.Extra
import qualified Hedgehog.Gen     as Gen
import qualified Hedgehog.Range   as Range
import qualified Prelude          as P

import           Hedgehog
import           Koan.Applicative as K
import           Koan.Functor     as K
import           Koan.List        as K
import           Koan.Monad       as K
import           Prelude          hiding (elem)

prop_head :: Property
prop_head = property $ do
  xs  <- forAll $ Gen.list (Range.linear 1 100) (Gen.int Range.constantBounded)
  K.head xs === P.head xs

prop_tail :: Property
prop_tail = property $ do
  xs  <- forAll $ Gen.list (Range.linear 1 100) (Gen.int Range.constantBounded)
  K.tail xs === P.tail xs

prop_last :: Property
prop_last = property $ do
  xs  <- forAll $ Gen.list (Range.linear 1 100) (Gen.int Range.constantBounded)
  K.last xs === P.last xs

prop_reverse :: Property
prop_reverse = property $ do
  xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.int Range.constantBounded)
  K.reverse xs === P.reverse xs

prop_concat_op :: Property
prop_concat_op = property $ do
  xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.int Range.constantBounded)
  ys  <- forAll $ Gen.list (Range.linear 0 100) (Gen.int Range.constantBounded)
  xs K.++ ys === xs P.++ ys

prop_concat :: Property
prop_concat = property $ do
  xs  <- forAll $ Gen.list (Range.linear 0 10) $ Gen.list (Range.linear 0 10) $ Gen.int Range.constantBounded
  K.concat xs === P.concat xs

prop_tails :: Property
prop_tails = property $ do
  xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.int Range.constantBounded)
  K.tails xs === P.tails xs

prop_map_list :: Property
prop_map_list = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) $ Gen.int Range.constantBounded
  K.mapList (*2) xs === P.fmap (*2) xs

prop_filter_list :: Property
prop_filter_list = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) $ Gen.int Range.constantBounded
  filter even xs === K.filterList even xs

prop_foldl_list :: Property
prop_foldl_list = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) $ Gen.int Range.constantBounded
  K.foldlList (+) 0 xs === foldl (+) 0 xs

prop_foldr_list :: Property
prop_foldr_list = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) $ Gen.int Range.constantBounded
  K.foldrList (+) 0 xs === foldr (+) 0 xs

prop_apply_list :: Property
prop_apply_list = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) $ Gen.int Range.constantBounded
  let fns = [(+ 2), (* 4), \x -> -x, mod 2]
  (fns K.<*> xs) === (fns P.<*> xs)

prop_bind_list :: Property
prop_bind_list = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) $ Gen.int Range.constantBounded
  (xs K.>>= replicate 5) === (xs P.>>= replicate 5)

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
