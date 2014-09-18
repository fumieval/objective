{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveDataTypeable #-}
module Control.Object where

import Control.Comonad.Zero
import Control.Comonad
import Control.Monad.Trans.State
import Control.Monad
import Data.Typeable

-- | The type 'Object e m' represents objects which can handle messages 'e', perform actions in the 'Monad' 'm'.
-- It can be thought of as a function where both domain and codomain are effects.
-- Thus, it can be composed just like functions using '(.>>.)' (not often needed); the identity element is `echo`.
newtype Object e m = Object { runObject :: forall x. e x -> m (x, Object e m) } deriving Typeable

-- | Change the workspace of the object.
transObject :: Monad n => (forall x. m x -> n x) -> Object e m -> Object e n
transObject f (Object m) = Object $ liftM (fmap (transObject f)) . f . m

-- | Apply a function to the messages coming into the object.
adaptObject :: Monad m => (forall x. e x -> f x) -> Object f m -> Object e m
adaptObject f (Object m) = Object $ liftM (fmap (adaptObject f)) . m . f

-- | Parrots messages given.
echo :: Functor e => Object e e
echo = Object (fmap (\x -> (x, echo)))

-- | Compose two objects (aka Dependency Injection).
(.>>.) :: Monad n => Object e m -> Object m n -> Object e n
Object m .>>. Object n = Object $ \e -> liftM (\((x, m'), n') -> (x, m' .>>. n')) $ n (m e)

-- | Build an object.
oneshot :: (Functor e, Monad m) => (forall a. e (m a) -> m a) -> Object e m
oneshot m = go where
  go = Object $ \e -> m (fmap return e) >>= \a -> return (a, go)
{-# INLINE oneshot #-}

-- | Build a stateful object.
stateful :: (Functor e, Monad m) => (forall a. e (StateT s m a) -> StateT s m a) -> s -> Object (AccessT s e) m
stateful m = go where
  go s = Object $ \k -> liftM (fmap go) $ case k of
    LiftAccessT e -> runStateT (m (fmap return e)) s
    Get cont -> return (cont s, s)
    Put s' cont -> return (cont, s')
{-# INLINE stateful #-}

-- | Like 'MonadState', but doesn't require 'Monad' as a prerequisite.
class Stateful s f where
  get_ :: f s
  put_ :: s -> f ()

-- | Inflicts external state accessibility to arbitrary effects.
data AccessT s f a = Get (s -> a) | Put s a | LiftAccessT (f a) deriving (Functor, Typeable)

instance Stateful s (AccessT s f) where
  get_ = Get id
  put_ s = Put s ()

variable :: Monad m => s -> Object (AccessT s Zero) m
variable s = Object $ \x -> case x of
  Get cont -> return (cont s, variable s)
  Put s' cont -> return (cont, variable s')
  LiftAccessT e -> return (extract e, variable s)