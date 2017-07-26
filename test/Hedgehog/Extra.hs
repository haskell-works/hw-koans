module Hedgehog.Extra where

import Hedgehog
import Hedgehog.Internal.Property (MonadTest(..), failDiff)
import Hedgehog.Internal.Source (HasCallStack(..), withFrozenCallStack)

reversed :: Group -> Group
reversed (Group name properties) = Group name (reverse properties)

(/==) :: (MonadTest m, Eq a, Show a, HasCallStack) => a -> a -> m ()
(/==) x y = do
  ok <- withFrozenCallStack $ eval (x /= y)
  if ok
    then success
    else withFrozenCallStack $ failDiff x y
