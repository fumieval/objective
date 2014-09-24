{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module Control.Monad.Objective.Class where
import Control.Object

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
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS
import Data.Monoid
import Control.Monad
import Control.Monad.Free

infix 3 .-
infix 3 .&

class Monad m => MonadObjective m where

  data Address (e :: * -> *) (m :: * -> *)
  type Residence (m :: * -> *) :: * -> *
  -- Control e f = Control e (Base f)
  -- | Send a message to the pointed one.
  (.-) :: Address e m -> e a -> m a
  -- | Add an object to the environment.
  new :: Object e (Residence m) -> m (Address e m)

-- | Old synonym for 'new'.
invoke :: MonadObjective m => Object e (Residence m) -> m (Address e m)
invoke = new
{-# DEPRECATED invoke "Use new instead of misleading invoke" #-} 

(.&) :: (MonadObjective m, Stateful s e) => Address e m -> Strict.StateT s m a -> m a
c .& m = do
  s <- c .- get_
  (a, s') <- Strict.runStateT m s
  c .- put_ s'
  return a

-- | like 'sequential', but it allows ad-hoc use of 'Free'.
(.|-) :: MonadObjective m => Address e m -> Free e a -> m a
_ .|- Pure a = return a
c .|- Free f = c .- f >>= (c .|-)

instance MonadObjective m => MonadObjective (ReaderT r m) where
  data Address e (ReaderT r m) = WrapReaderT (Address e m)
  type Residence (ReaderT r m) = Residence m
  WrapReaderT c .- e = lift (c .- e)
  new = liftM WrapReaderT . lift . new

instance (Monoid w, MonadObjective m) => MonadObjective (LazyRWS.RWST r w s m) where
  data Address e (LazyRWS.RWST r w s m) = WrapLazyRWST (Address e m)
  type Residence (LazyRWS.RWST r w s m) = Residence m
  WrapLazyRWST c .- e = lift (c .- e)
  new = liftM WrapLazyRWST . lift . new

instance (Monoid w, MonadObjective m) => MonadObjective (StrictRWS.RWST r w s m) where
  data Address e (StrictRWS.RWST r w s m) = WrapStrictRWST (Address e m)
  type Residence (StrictRWS.RWST r w s m) = Residence m
  WrapStrictRWST c .- e = lift (c .- e)
  new = liftM WrapStrictRWST . lift . new

instance MonadObjective m => MonadObjective (ContT r m) where
  data Address e (ContT r m) = WrapContT (Address e m)
  type Residence (ContT r m) = Residence m
  WrapContT c .- e = lift (c .- e)
  new = liftM WrapContT . lift . new

#if MIN_VERSION_transformers(0,4,0)
instance MonadObjective m => MonadObjective (ExceptT er m) where
  data Address e (ExceptT er m) = WrapExceptT (Address e m)
  type Residence (ExceptT er m) = Residence m
  WrapExceptT c .- e = lift (c .- e)
  new = liftM WrapExceptT . lift . new
#else
instance (Error er, MonadObjective m) => MonadObjective (ErrorT er m) where
  data Address e (ErrorT er m) = WrapErrorT (Address e m)
  type Residence (ErrorT er m) = Residence m
  WrapErrorT c .- e = lift (c .- e)
  new = liftM WrapErrorT . lift . new
#endif

instance MonadObjective m => MonadObjective (IdentityT m) where
  data Address e (IdentityT m) = WrapIdentityT (Address e m)
  type Residence (IdentityT m) = Residence m
  WrapIdentityT c .- e = lift (c .- e)
  new = liftM WrapIdentityT . lift . new

instance MonadObjective m => MonadObjective (ListT m) where
  data Address e (ListT m) = WrapListT (Address e m)
  type Residence (ListT m) = Residence m
  WrapListT c .- e = lift (c .- e)
  new = liftM WrapListT . lift . new

instance MonadObjective m => MonadObjective (MaybeT m) where
  data Address e (MaybeT m) = WrapMaybeT (Address e m)
  type Residence (MaybeT m) = Residence m
  WrapMaybeT c .- e = lift (c .- e)
  new = liftM WrapMaybeT . lift . new

instance MonadObjective m => MonadObjective (Lazy.StateT s m) where
  data Address e (Lazy.StateT s m) = WrapLazyStateT (Address e m)
  type Residence (Lazy.StateT s m) = Residence m
  WrapLazyStateT c .- e = lift (c .- e)
  new = liftM WrapLazyStateT . lift . new

instance MonadObjective m => MonadObjective (Strict.StateT s m) where
  data Address e (Strict.StateT s m) = WrapStrictStateT (Address e m)
  type Residence (Strict.StateT s m) = Residence m
  WrapStrictStateT c .- e = lift (c .- e)
  new = liftM WrapStrictStateT . lift . new

instance (Monoid w, MonadObjective m) => MonadObjective (Lazy.WriterT w m) where
  data Address e (Lazy.WriterT w m) = WrapLazyWriterT (Address e m)
  type Residence (Lazy.WriterT w m) = Residence m
  WrapLazyWriterT c .- e = lift (c .- e)
  new = liftM WrapLazyWriterT . lift . new

instance (Monoid w, MonadObjective m) => MonadObjective (Strict.WriterT w m) where
  data Address e (Strict.WriterT w m) = WrapStrictWriterT (Address e m)
  type Residence (Strict.WriterT w m) = Residence m
  WrapStrictWriterT c .- e = lift (c .- e)
  new = liftM WrapStrictWriterT . lift . new
