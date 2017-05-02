{-# LANGUAGE InstanceSigs #-}

module Koan.State where

enrolled :: Bool
enrolled = False

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State run) = State (\s -> let (a, t) = run s in (f a, t))

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State (\s -> (a, s))

    (<*>) :: State s (a -> b) -> State s a -> State s b
    (<*>) (State runF) (State runA) = State (\s ->
      let (f, t) = runF s
          (a, u) = runA t
          in (f a, u))

instance Monad (State s) where
    (>>=) :: State s a -> (a -> State s b) -> State s b
    (>>=) (State run) f = State (\s -> let (a, t) = run s in let State run2 = f a in run2 t)

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put a = State (const ((), a))

modify :: (s -> s) -> State s ()
modify f = get >>= (put . f)
