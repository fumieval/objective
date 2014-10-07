{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor, DeriveDataTypeable #-}
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE TypeOperators, TupleSections, GADTs #-}
module Control.Object (
  -- * Construction
  Object(..),
  liftO,
  echo,
  oneshot,
  stateful,
  variable,
  -- * Composition
  (.>>.),
  transObject,
  adaptObject,
  sequential,
  -- * Extensible objects
  loner,
  (.|>.),
  sharing,
  -- * Utilitites
  Request(..),
  request,
  accept,
  acceptM,
  Lift(..),
  get_,
  modify_,
  put_,
  )
where

import Control.Monad.Trans.State.Strict
import Control.Monad
import Data.Typeable
import Control.Applicative
import Control.Monad.Free
import Data.OpenUnion1.Clean

-- | The type 'Object e m' represents objects which can handle messages @e@, perform actions in the environment @m@.
-- It can be thought of as an automaton that converts effects.
-- 'Object's can be composed just like functions using '.>>.'; the identity element is 'echo'.
newtype Object e m = Object { runObject :: forall x. e x -> m (x, Object e m) } deriving Typeable

-- | Lift a natural transformation into an object.
liftO :: Functor f => (forall x. e x -> f x) -> Object e f
liftO f = Object $ fmap (\x -> (x, liftO f)) . f

-- | Change the workspace of the object.
transObject :: Functor g => (forall x. f x -> g x) -> Object e f -> Object e g
transObject f (Object m) = Object $ fmap (fmap (transObject f)) . f . m

-- | Apply a function to the messages coming into the object.
adaptObject :: Functor m => (forall x. e x -> f x) -> Object f m -> Object e m
adaptObject f (Object m) = Object $ fmap (fmap (adaptObject f)) . m . f

-- | Parrots messages given.
echo :: Functor e => Object e e
echo = Object (fmap (\x -> (x, echo)))

-- | Compose two objects (aka Dependency Injection).
(.>>.) :: Functor n => Object e m -> Object m n -> Object e n
Object m .>>. Object n = Object $ \e -> fmap (\((x, m'), n') -> (x, m' .>>. n')) $ n (m e)

infixr 4 .>>.

-- | Build an object using continuation passing style.
oneshot :: (Functor e, Monad m) => (forall a. e (m a) -> m a) -> Object e m
oneshot m = go where
  go = Object $ \e -> m (fmap return e) >>= \a -> return (a, go)
{-# INLINE oneshot #-}

-- | Build a stateful object.
stateful :: Monad m => (forall a. e a -> StateT s m a) -> s -> Object e m
stateful h = go where
  go s = Object $ liftM (\(a, s') -> (a, go s')) . flip runStateT s . h
{-# INLINE stateful #-}

-- | Convert a /method sequence/ into a sequential /method execution/.
sequential :: Monad m => Object e m -> Object (Free e) m
sequential obj = Object $ \x -> case x of
  Pure a -> return (a, sequential obj)
  Free f -> do
    (a, obj') <- runObject obj f
    runObject (sequential obj') a

-- | A mutable variable.
variable :: Applicative f => s -> Object (State s) f
variable s = Object $ \m -> let (a, s') = runState m s in pure (a, variable s')

-- | Build a stateful object, sharing out the state.
sharing :: Monad m => (forall a. e a -> StateT s m a) -> s -> Object (State s |> e |> Nil) m
sharing m = go where
  go s = Object $ \k -> liftM (fmap go) $ ($k)
    $ (\n -> return $ runState n s)
    ||> (\e -> runStateT (m e) s)
    ||> exhaust
{-# INLINE sharing #-}

-- | An object that won't accept any messages.
loner :: Functor m => Object Nil m
loner = liftO exhaust

-- | Extend an object by adding another independent object.
(.|>.) :: Functor m => Object f m -> Object (Union s) m -> Object (f |> Union s) m
p .|>. q = Object $ fmap (fmap (.|>.q)) . runObject p ||> fmap (fmap (p .|>.)) . runObject q

data Request a b r = Request a (b -> r)

class Lift f g | g -> f where
  lift_ :: f a -> g a

instance Lift (Request a b) (Request a b) where
  lift_ = id

instance (f ∈ u) => Lift f (Union u) where
  lift_ = liftU

instance Lift (StateT s m) (StateT s m) where
  lift_ = id

get_ :: (Monad m, Lift (StateT s m) f) => f s
get_ = lift_ get

modify_ :: (Monad m, Lift (StateT s m) f) => (s -> s) -> f ()
modify_ f = lift_ (modify f)

put_ :: (Monad m, Lift (StateT s m) f) => s -> f ()
put_ s = lift_ (put s)

request :: (Lift (Request a b) f) => a -> f b
request a = lift_ (Request a id)

accept :: Functor f => (a -> f b) -> Request a b r -> f r
accept f (Request a br) = fmap br (f a)

acceptM :: Monad m => (a -> m b) -> Request a b r -> m r
acceptM f (Request a br) = liftM br (f a)
