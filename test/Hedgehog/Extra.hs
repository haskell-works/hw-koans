module Hedgehog.Extra where

import           Hedgehog

reversed :: Group -> Group
reversed (Group name properties) = Group name (reverse properties)
