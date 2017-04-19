module Main where

import           Control.Monad
import           Data.Monoid
import qualified Koan.Applicative
import           Lib

tests =
  [ Koan.Applicative.tests
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
