{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveDataTypeable #-}
module Control.Object where

import Control.Comonad.Zero
import Control.Comonad
import Control.Monad.Trans.State
import Control.Monad
import Data.Typeable

newtype Object e m = Object { runObject :: forall x. e x -> m (x, Object e m) } deriving Typeable

oneshot :: (Functor e, Monad m) => (forall a. e (m a) -> m a) -> Object e m
oneshot m = go where
  go = Object $ \e -> m (fmap return e) >>= \a -> return (a, go)
{-# INLINE oneshot #-}

stateful :: (Functor e, Monad m) => (forall a. e (StateT s m a) -> StateT s m a) -> s -> Object (AccessT s e) m
stateful m = go where
  go s = Object $ \k -> liftM (fmap go) $ case k of
    LiftAccessT e -> runStateT (m (fmap return e)) s
    Get cont -> return (cont s, s)
    Put s' cont -> return (cont, s')
{-# INLINE stateful #-}

class Stateful s f where
  get_ :: f s
  put_ :: s -> f ()

data AccessT s f a = Get (s -> a) | Put s a | LiftAccessT (f a) deriving (Functor, Typeable)

instance Stateful s (AccessT s f) where
  get_ = Get id
  put_ s = Put s ()

variable :: Monad m => s -> Object (AccessT s Zero) m
variable s = Object $ \x -> case x of
  Get cont -> return (cont s, variable s)
  Put s' cont -> return (cont, variable s')
  LiftAccessT e -> return (extract e, variable s)