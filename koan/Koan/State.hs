{-# LANGUAGE InstanceSigs #-}

module Koan.State where

enrolled :: Bool
enrolled = False

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap = error "TODO: Implement fmap in Functor (State s) instance"

instance Applicative (State s) where
    pure :: a -> State s a
    pure = error "TODO: Implement pure in Applicative (State s) instance"

    (<*>) :: State s (a -> b) -> State s a -> State s b
    (<*>) = error "TODO: Implement (<*>) in Applicative (State s) instance"

instance Monad (State s) where
    (>>=) :: State s a -> (a -> State s b) -> State s b
    (>>=) = error "TODO: Implement (>>=) in Monad (State s) instance"

get :: State s s
get = error "TODO: Implement get"

put :: s -> State s ()
put = error "TODO: Implement put"

modify :: (s -> s) -> State s ()
modify = error "TODO: Implement modify"
