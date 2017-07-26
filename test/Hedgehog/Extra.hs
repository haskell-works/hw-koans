module Hedgehog.Extra where

import Hedgehog
import Hedgehog.Internal.Property (MonadTest(..), failDiff, failWith)
import Hedgehog.Internal.Source (HasCallStack(..), withFrozenCallStack)
import Hedgehog.Internal.Show

reversed :: Group -> Group
reversed (Group name properties) = Group name (reverse properties)

(/==) :: (MonadTest m, Eq a, Show a, HasCallStack) => a -> a -> m ()
(/==) x y = do
  ok <- withFrozenCallStack $ eval (x /= y)
  if ok
    then success
    else withFrozenCallStack $ failDiff x y

(?==) :: (MonadTest m, Eq a, Show a, HasCallStack) => a -> (a -> Bool) -> m ()
(?==) x p = do
  r <- withFrozenCallStack $ eval x
  if p r
    then success
    else withFrozenCallStack $ failWith Nothing $ unlines
          [ "━━━ Invalid result ━━━"
          , showPretty x
          ]
