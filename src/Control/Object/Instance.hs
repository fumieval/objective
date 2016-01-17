{-# LANGUAGE GADTs, Rank2Types, LambdaCase #-}
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
  Instance
  , new
  , newSettle
  -- * Invocation
  , invokeOn
  , (.-)
  , (?-)
  ) where
import Control.Concurrent
import Control.Exception (evaluate)
import Control.Object.Object
import Control.Monad.IO.Class
import Control.Monad.Catch

type Instance f g = MVar (Object f g)

-- | Invoke a method with an explicit landing function.
-- In case of exception, the original object will be set.
invokeOn :: (MonadIO m, MonadMask m)
         => (forall x. g x -> m x) -> MVar (Object f g) -> f a -> m a
invokeOn m v f = mask $ \restore -> do
  obj <- liftIO $ takeMVar v
  (a, obj') <- restore (m (runObject obj f) >>= liftIO . evaluate) `onException` liftIO (putMVar v obj)
  liftIO $ putMVar v obj'
  return a

-- | Invoke a method.
(.-) :: (MonadIO m, MonadMask m) => MVar (Object f m) -> f a -> m a
(.-) = invokeOn id
{-# INLINE (.-) #-}
infixr 3 .-

-- | Try to invoke a method. If the instance is unavailable, it returns Nothing.
(?-) :: (MonadIO m, MonadMask m) => MVar (Object f m) -> f a -> m (Maybe a)
v ?- f = mask $ \restore -> liftIO (tryTakeMVar v) >>= \case
  Just obj -> do
    (a, obj') <- restore (runObject obj f >>= liftIO . evaluate) `onException` liftIO (putMVar v obj)
    liftIO $ putMVar v obj'
    return (Just a)
  Nothing -> return Nothing

-- | Create a new instance. This can be used inside 'unsafePerformIO' to create top-level instances.
new :: MonadIO m => Object f g -> m (Instance f g)
new = liftIO . newMVar
{-# INLINE new #-}

-- | Create a new instance, having it sitting on the current environment.
newSettle :: MonadIO m => Object f m -> m (Instance f m)
newSettle = new
{-# INLINE newSettle #-}
