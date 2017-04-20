module Koan.Functor where

import           Prelude hiding (Functor, fmap, (<$), (<$>))

enrolled :: Bool
enrolled = False

{- | The 'Functor' class is used for types that can be mapped over.
Instances of 'Functor' should satisfy the following laws:

> fmap id  ==  id
> fmap (f . g)  ==  fmap f . fmap g

The instances of 'Functor' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
satisfy these laws.
-}

class Functor f  where
  fmap :: (a -> b) -> f a -> f b

  -- | Replace all locations in the input with the same value.
  -- The default definition is @'fmap' . 'const'@, but this may be
  -- overridden with a more efficient version.
  (<$) :: a -> f b -> f a
  (<$) = error "TODO: implement (<$)"

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = error "TODO: implement (<$>)"

($>) :: Functor f => f a -> b -> f b
($>) = error "TODO: implement ($>)"

void :: Functor f => f a -> f ()
void = error "TODO: implement void"
