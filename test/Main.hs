module Main where

import qualified Check.Applicative
import qualified Check.Eq
import qualified Check.Functor
import qualified Check.Ord
import qualified Check.Start
import           Control.Monad
import           Data.Monoid
import           Lib

tests =
  [ Check.Applicative.tests
  , Check.Eq.tests
  , Check.Functor.tests
  , Check.Ord.tests
  , Check.Start.tests
  ]

main :: IO ()
main = do
  results <- forM tests id
  let successes = countElem True results
  let failures  = countElem False results
  let suites = successes + failures
  if failures == 0
    then putStrLn $ "All test suites succeeded"
    else putStrLn $ show failures <> " out of " <> show suites <> " test suites failed"
