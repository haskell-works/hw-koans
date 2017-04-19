module Main where

import           Control.Monad
import           Data.Monoid
import qualified Koan.Applicative
import qualified Koan.Eq
import qualified Koan.Functor
import qualified Koan.Ord
import           Lib

tests =
  [ Koan.Applicative.tests
  , Koan.Eq.tests
  , Koan.Functor.tests
  , Koan.Ord.tests
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
