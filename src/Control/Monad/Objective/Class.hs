{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Objective.Class where
import Control.Object
import Control.Monad.Trans.State

infix 3 .-
infix 3 .&

class Monad m => MonadObjective s m where
  type Base m :: * -> *
  data Control s (e :: * -> *)
  (.-) :: Control s e -> e a -> m a
  invoke :: Object e (Base m) -> m (Control s e)

(.&) :: (MonadObjective k m, Stateful s e) => Control k e -> StateT s m a -> m a
c .& m = do
  s <- c .- get_
  (a, s') <- runStateT m s
  c .- put_ s'
  return a