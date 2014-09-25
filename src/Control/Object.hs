{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor, DeriveDataTypeable #-}
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}
module Control.Object where

import Control.Comonad.Zero
import Control.Comonad
import Control.Monad.Trans.State
import Control.Monad
import Data.Typeable
import Control.Applicative
import Data.Maybe
import Control.Monad.Free
import Control.Monad.Trans.Maybe

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

-- | Build an object.
oneshot :: (Functor e, Monad m) => (forall a. e (m a) -> m a) -> Object e m
oneshot m = go where
  go = Object $ \e -> m (fmap return e) >>= \a -> return (a, go)
{-# INLINE oneshot #-}

-- | Build a stateful object.
stateful :: Monad m => (forall a. e a -> StateT s m a) -> s -> Object e m
stateful h = go where
  go s = Object $ liftM (\(a, s) -> (a, go s)) . flip runStateT s . h

-- | Build a stateful object, sharing out the state.
sharing :: Monad m => (forall a. e a -> StateT s m a) -> s -> Object (AccessT s e) m
sharing m = go where
  go s = Object $ \k -> liftM (fmap go) $ case k of
    LiftAccessT e -> runStateT (m e) s
    Get cont -> return (cont s, s)
    Put s' cont -> return (cont, s')
{-# INLINE sharing #-}

-- | Like 'MonadState', but doesn't require 'Monad' as a prerequisite.
class Stateful s f | f -> s where
  get_ :: f s
  put_ :: s -> f ()

-- | Inflicts external state accessibility to arbitrary effects.
data AccessT s f a = Get (s -> a) | Put s a | LiftAccessT (f a) deriving (Functor, Typeable)

instance Stateful s (AccessT s f) where
  get_ = Get id
  put_ s = Put s ()

instance (Functor f, Stateful s f) => Stateful s (Free f) where
  get_ = liftF get_
  put_ = liftF . put_

-- | A mutable variable.
variable :: Applicative f => s -> Object (Access s) f
variable s = Object $ \x -> case x of
  Get cont -> pure (cont s, variable s)
  Put s' cont -> pure (cont, variable s')
  LiftAccessT e -> pure (extract e, variable s)

type Access s = AccessT s Zero

-- | Convert a /method sequence/ into a sequential /method execution/.
sequential :: Monad m => Object e m -> Object (Free e) m
sequential obj = Object $ \x -> case x of
  Pure a -> return (a, sequential obj)
  Free f -> do
    (a, obj') <- runObject obj f
    runObject (sequential obj') a
