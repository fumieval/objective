{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor, DeriveDataTypeable #-}
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
module Control.Object where

import Control.Comonad.Zero
import Control.Comonad
import Control.Monad.Trans.State.Strict
import Control.Monad
import Data.Typeable
import Control.Applicative
import Data.Maybe
import Control.Monad.Free
import Control.Monad.Trans.Maybe
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

-- | Build an object.
oneshot :: (Functor e, Monad m) => (forall a. e (m a) -> m a) -> Object e m
oneshot m = go where
  go = Object $ \e -> m (fmap return e) >>= \a -> return (a, go)
{-# INLINE oneshot #-}

-- | Build a stateful object.
stateful :: Monad m => (forall a. e a -> StateT s m a) -> s -> Object e m
stateful h = go where
  go s = Object $ liftM (\(a, s) -> (a, go s)) . flip runStateT s . h
{-# INLINE stateful #-}

-- | Convert a /method sequence/ into a sequential /method execution/.
sequential :: Monad m => Object e m -> Object (Free e) m
sequential obj = Object $ \x -> case x of
  Pure a -> return (a, sequential obj)
  Free f -> do
    (a, obj') <- runObject obj f
    runObject (sequential obj') a

-- | Build a stateful object, sharing out the state.
sharing :: Monad m => (forall a. e a -> StateT s m a) -> s -> Object (StateT s m |> e |> Nil) m
sharing m = go where
  go s = Object $ \k -> liftM (fmap go) $ ($k)
    $ (\n -> runStateT n s)
    ||> (\e -> runStateT (m e) s)
    ||> exhaust
{-# INLINE sharing #-}

-- | A mutable variable.
variable :: Applicative f => s -> Object (State s) f
variable s = Object $ \m -> let (a, s') = runState m s in pure (a, variable s')

-- | aka indexed store comonad
data Request a b r = Request a (b -> r) deriving Functor

class Requestable a b f | f -> a, f -> b where
  request :: a -> f b

instance Requestable a b (Request a b) where
  request a = Request a id

instance (Requestable a b f, f ∈ u) => Requestable a b (Union u) where
  request = liftU . request

class Stateful s f | f -> s where
  get_ :: f s
  modify_ :: (s -> s) -> f ()

put_ :: Stateful s f => s -> f ()
put_ s = modify_ (const s)

instance Monad m => Stateful s (StateT s m) where
  get_ = get
  modify_ = modify

instance (Stateful s f, f ∈ u) => Stateful s (Union u) where
  get_ = liftU get_
  modify_ = liftU . modify_