{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs, Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Object.Instance
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  GADTs, Rank2Types
--
-----------------------------------------------------------------------------
module Control.Object.Instance (
  -- * Instantiation
  Instance(..)
  , new
  , newSettle
  , newSTM
  -- * Invocation
  , invokeOn
  , invokeOnSTM
  , (.-)
  , (..-)
  , snapshot
  ) where
import Control.Concurrent.STM.TMVar
import Control.Monad.STM
import Control.Object.Object
import Control.Monad.IO.Class
import Control.Monad.STM.Class
import Control.Monad
import Control.Monad.Catch (MonadMask, bracketOnError)

-- | TMVar-based instance
data Instance f g where
  InstRef :: (forall x. e x -> f x) -> (forall x. g x -> h x) -> TMVar (Object f g) -> Instance e h

instance HProfunctor Instance where
  f ^>>@ InstRef l r v = InstRef (l . f) r v
  {-# INLINE (^>>@) #-}
  InstRef l r v @>>^ f = InstRef l (f . r) v
  {-# INLINE (@>>^) #-}

-- | Invoke a method with an explicit landing function.
invokeOn :: (MonadIO m, MonadMask m)
         => (forall x. g x -> m x) -> Instance f g -> f a -> m a
invokeOn m (InstRef t l v) f = bracketOnError
  (liftIO $ atomically $ takeTMVar v)
  (\obj -> liftIO $ atomically $ do
    _ <- tryTakeTMVar v
    putTMVar v obj)
  (\obj -> do
    (a, obj') <- m $ l $ runObject obj (t f)
    liftIO $ atomically $ putTMVar v obj'
    return a)

-- | Invoke a method with an explicit landing function.
invokeOnSTM :: (forall x. g x -> STM x) -> Instance f g -> f a -> STM a
invokeOnSTM m (InstRef t l v) f = do
  obj <- takeTMVar v
  (a, obj') <- m $ l $ runObject obj (t f)
  putTMVar v obj'
  return a

-- | Invoke a method, atomically.
(..-) :: MonadSTM m => Instance f STM -> f a -> m a
(..-) i = liftSTM . invokeOnSTM id i
{-# INLINE (..-) #-}
infixr 3 ..-

-- | Invoke a method.
(.-) :: (MonadIO m, MonadMask m) => Instance f m -> f a -> m a
(.-) = invokeOn id
{-# INLINE (.-) #-}
infixr 3 .-

-- | Take a snapshot of an instance.
snapshot :: (MonadSTM m, Functor g) => Instance f g -> m (Object f g)
snapshot (InstRef f g v) = liftSTM $ go `fmap` takeTMVar v
  where go (Object m) = Object $ fmap (fmap go) . g . m . f

-- | Create a new instance. This can be used inside 'unsafePerformIO' to create top-level instances.
new :: MonadIO m => Object f g -> m (Instance f g)
new = liftIO . liftM (InstRef id id) . newTMVarIO
{-# INLINE new #-}

-- | Create a new instance, having it sitting on the current environment.
newSettle :: MonadIO m => Object f m -> m (Instance f m)
newSettle = new
{-# INLINE newSettle #-}

-- | Create a new instance.
newSTM :: MonadSTM m => Object f g -> m (Instance f g)
newSTM = liftSTM . liftM (InstRef id id) . newTMVar
{-# INLINE newSTM #-}
