module Main where

import qualified Check.Applicative
import qualified Check.Eq
import qualified Check.Functor
import qualified Check.List
import qualified Check.Maybe
import qualified Check.Ord
import qualified Check.Simple
import qualified Check.Start
import qualified Check.State
import           Control.Monad
import           Data.Maybe
import           Data.Monoid
import qualified Koan
import qualified Koan.Applicative
import qualified Koan.Eq
import qualified Koan.Functor
import qualified Koan.List
import qualified Koan.Maybe
import qualified Koan.Ord
import qualified Koan.Simple
import qualified Koan.Start
import qualified Koan.State

{- | Returns a count of the number of times the given element occured in the
given list. -}
countElem :: Eq a => a -> [a] -> Int
countElem i = length . filter (i==)

tests =
  [ (Koan.Applicative.enrolled  , Check.Applicative.tests )
  , (Koan.Eq.enrolled           , Check.Eq.tests          )
  , (Koan.Functor.enrolled      , Check.Functor.tests     )
  , (Koan.List.enrolled         , Check.List.tests        )
  , (Koan.Maybe.enrolled        , Check.Maybe.tests       )
  , (Koan.Ord.enrolled          , Check.Ord.tests         )
  , (Koan.Simple.enrolled       , Check.Simple.tests      )
  , (Koan.Start.enrolled        , Check.Start.tests       )
  , (Koan.State.enrolled        , Check.State.tests       )
  ]

main :: IO ()
main = do
  results <- forM tests $ \(enrolled, test) -> do
    if enrolled || Koan.allEnrolled
      then Just <$> test else return Nothing
  let suites = catMaybes results
  let numSuites = length suites
  let numSuccesses    = countElem True suites
  let numFailures     = countElem False suites
  let numNotEnrolled  = countElem Nothing results
  putStrLn ""
  putStrLn ""
  if numFailures == 0
    then putStrLn $ "All enrolled " <> show numSuites <> " test suites succeeded"
    else putStrLn $ show numFailures <> " out of " <> show numSuites <> " test suites failed"
  when (numNotEnrolled > 0) $ do
    putStrLn $ show numNotEnrolled <> " suites not enrolled"
