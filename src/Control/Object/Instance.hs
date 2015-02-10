{-# LANGUAGE GADTs, Rank2Types #-}
module Control.Object.Instance where
import Control.Concurrent.MVar
import Control.Object.Object
import Control.Monad.IO.Class
import Control.Monad

data Instance f g where
  InstRef :: MVar (Object f g) -> Instance f g
  InstLmap :: (forall x. f x -> g x) -> Instance g h -> Instance f h
  InstRmap :: Instance f g -> (forall x. g x -> h x) -> Instance f h

instance HProfunctor Instance where
  (^>>@) = InstLmap
  (@>>^) = InstRmap

invoke :: MonadIO m => (forall x. g x -> m x) -> Instance f g -> f a -> m a
invoke m (InstRef v) f = do
  obj <- liftIO (takeMVar v)
  (a, obj') <- m (runObject obj f)
  liftIO $ putMVar v obj'
  return a
invoke m (InstLmap t i) f = invoke m i (t f)
invoke m (InstRmap i t) f = invoke (m . t) i f

(.-) :: MonadIO m => Instance f m -> f a -> m a
(.-) = invoke id

new :: MonadIO m => Object f g -> m (Instance f g)
new = liftIO . liftM InstRef . newMVar
