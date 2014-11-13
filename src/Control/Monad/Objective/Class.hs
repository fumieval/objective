{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Control.Monad.Objective.Class where
import Control.Object
{-
#if MIN_VERSION_transformers(0,4,0)
import Control.Monad.Trans.Except
#else
import Control.Monad.Trans.Error
#endif
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
import Data.Monoid
import Data.OpenUnion1.Clean
import Control.Monad.Free
-}
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
