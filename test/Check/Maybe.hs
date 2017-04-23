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
import           Koan.Maybe          as K
import           Prelude             hiding (elem, filter)

prop_orElse :: Property
prop_orElse = property $ do
  ma  <- forAll $ Gen.maybe (Gen.int (Range.constantBounded))
  b   <- forAll $ Gen.int (Range.constantBounded)
  (ma `K.orElse` b) === (fromMaybe b ma)

prop_orMaybe :: Property
prop_orMaybe = property $ do
  ma  <- forAll $ Gen.maybe (Gen.int (Range.constantBounded))
  mb  <- forAll $ Gen.maybe (Gen.int (Range.constantBounded))
  (ma `K.orMaybe` mb) === (ma <|> mb)

prop_mapMaybe :: Property
prop_mapMaybe = property $ do
  mb  <- forAll $ Gen.maybe (Gen.int (Range.constantBounded))
  ((+1) `K.mapMaybe` mb) === ((+1) <$> mb)

prop_concatMaybes :: Property
prop_concatMaybes = property $ do
  ma  <- forAll $ Gen.list ((Range.linear 1 100)) (Gen.maybe (Gen.int (Range.constantBounded)))
  (K.concatMaybes ma) === (catMaybes ma)

prop_filterMaybe :: Property
prop_filterMaybe = property $ do
  ma  <- forAll $ Gen.maybe (Gen.int (Range.constantBounded))
  (K.filterMaybe even ma) === listToMaybe (P.filter even (catMaybes [ma]))

prop_foldMaybe :: Property
prop_foldMaybe = property $ do
  ma  <- forAll $ Gen.maybe (Gen.int (Range.constantBounded))
  (K.foldMaybe (+) 0 ma) === (P.foldr (+) 0 ma)

prop_applyMaybe :: Property
prop_applyMaybe = property $ do
  let mf1 = Nothing :: Maybe (Int -> Int)
  let mf2 = Just (+1)
  ma  <- forAll $ Gen.maybe (Gen.int (Range.constantBounded))
  (K.applyMaybe mf1 ma) === (mf1 P.<*> ma)
  (K.applyMaybe mf2 ma) === (mf2 P.<*> ma)

prop_bindMaybe :: Property
prop_bindMaybe = property $ do
  let mf1 = (\a -> Nothing) :: Int -> Maybe Int
  let mf2 = (\a -> Just 1)  :: Int -> Maybe Int
  ma  <- forAll $ Gen.maybe (Gen.int (Range.constantBounded))
  (K.bindMaybe mf1 ma) === (ma P.>>= mf1)
  (K.bindMaybe mf2 ma) === (ma P.>>= mf2)

tests :: IO Bool
tests = ($$(checkConcurrent))
