{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Control.Monad.Objective.Class where
import Control.Object

import Control.Monad
import Control.Monad.Trans.State.Strict as Strict
infix 3 .-
infix 3 .^
infix 3 .&

type Address' e m = Address e m m

class Monad m => MonadObjective m where
  data Address (e :: * -> *) (n :: * -> *) m
  -- | Send a message to the pointed one.
  invoke :: Monad n => Address e n m -> e a -> m (n (m a))
  -- | Add an object to the environment.
  new :: Object e n -> m (Address e n m)

(.-) :: MonadObjective m => Address' e m -> e a -> m a
a .- e = join $ join $ invoke a e

(.^) :: (MonadObjective m, Lift e f) => Address' f m -> e a -> m a
a .^ e = a .- lift_ e

(.&) :: (MonadObjective m, Lift (Strict.State s) f) => Address' f m -> Strict.StateT s m a -> m a
c .& m = do
  s <- c .- get_
  (a, s') <- Strict.runStateT m s
  c .- put_ s'
  return a

pipeline :: (MonadObjective m, Lift (Strict.State s) f) => Address' f m -> Object e (Strict.StateT s m) -> Object e m
pipeline addr = go where
  go o = Object $ \e -> do
    s <- addr .- get_
    ((a, o'), s') <- runStateT (runObject o e) s
    addr .& put_ s'
    return (a, pipeline addr o')
