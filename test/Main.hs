module Main where

import Control.Monad
import Data.Maybe
import Data.Monoid

import qualified Check.Alternative
import qualified Check.Applicative
import qualified Check.Either
import qualified Check.Eq
import qualified Check.Function
import qualified Check.Functor
import qualified Check.GeoFeedParser
import qualified Check.List
import qualified Check.Maybe
import qualified Check.Monad
import qualified Check.Ord
import qualified Check.Reader
import qualified Check.Simple
import qualified Check.Start
import qualified Check.State
import qualified Koan
import qualified Koan.Alternative
import qualified Koan.Applicative
import qualified Koan.Either
import qualified Koan.Eq
import qualified Koan.Function
import qualified Koan.Functor
import qualified Koan.GeoFeedParser
import qualified Koan.List
import qualified Koan.Maybe
import qualified Koan.Monad
import qualified Koan.Ord
import qualified Koan.Reader
import qualified Koan.Simple
import qualified Koan.Start
import qualified Koan.State

{- | Returns a count of the number of times the given element occured in the
given list. -}
countElem :: Eq a => a -> [a] -> Int
countElem i = length . filter (i==)

tests =
  [ (Koan.Alternative.enrolled        , Check.Alternative.tests       )
  , (Koan.Applicative.enrolled        , Check.Applicative.tests       )
  , (Koan.Either.enrolled             , Check.Either.tests            )
  , (Koan.Eq.enrolled                 , Check.Eq.tests                )
  , (Koan.Function.enrolled           , Check.Function.tests          )
  , (Koan.Functor.enrolled            , Check.Functor.tests           )
  , (Koan.List.enrolled               , Check.List.tests              )
  , (Koan.Maybe.enrolled              , Check.Maybe.tests             )
  , (Koan.Monad.enrolled              , Check.Monad.tests             )
  , (Koan.Ord.enrolled                , Check.Ord.tests               )
  , (Koan.Reader.enrolled             , Check.Reader.tests            )
  , (Koan.Simple.enrolled             , Check.Simple.tests            )
  , (Koan.Start.enrolled              , Check.Start.tests             )
  , (Koan.State.enrolled              , Check.State.tests             )
  , (Koan.GeoFeedParser.enrolled      , Check.GeoFeedParser.tests     )
  ]

main :: IO ()
main = do
  results <- forM tests $ \(enrolled, test) ->
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
  when (numNotEnrolled > 0) $
    putStrLn $ show numNotEnrolled <> " suites not enrolled"
