{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveDataTypeable #-}
module Control.Object where

import Control.Comonad.Zero
import Control.Comonad
import Control.Monad.Trans.State
import Control.Monad
import Data.Typeable

-- | The type 'Object e m' represents objects which can handle messages 'e', perform actions in the environment 'm'.
-- It can be thought of as a function between effects.
-- Thus, it can be composed just like functions using '(.>>.)' (not often needed); the identity element is `echo`.
newtype Object e m = Object { runObject :: forall x. e x -> m (x, Object e m) } deriving Typeable

-- | Lift a natural transformation into an object.
liftO :: Functor f => (forall x. e x -> f x) -> Object e f
liftO f = Object $ fmap (\x -> (x, liftO f)) . f

-- | Change the workspace of the object.
transObject :: Functor g => (forall x. f x -> g x) -> Object e f -> Object e g
transObject f (Object m) = Object $ fmap (fmap (transObject f)) . f . m

-- | Apply a function to the messages coming into the object.
adaptObject :: Functor f => (forall x. e x -> f x) -> Object f m -> Object e m
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

-- | A mutable variable.
variable :: Applicative f => s -> Object (AccessT s Zero) f
variable s = Object $ \x -> case x of
  Get cont -> pure (cont s, variable s)
  Put s' cont -> pure (cont, variable s')
  LiftAccessT e -> pure (extract e, variable s)