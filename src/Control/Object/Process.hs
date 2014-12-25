{-# LANGUAGE Trustworthy #-}
module Control.Object.Process where
import Control.Object.Object
import Control.Arrow as A
import qualified Control.Category as C
import Data.Profunctor
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Functor.Request

-- | An object which is specialized to be a Mealy machine
newtype Process m a b = Process { unProcess :: Object (Request a b) m }

-- | @_Process :: Iso' (Object (Request a b) m) (Process m a b)@
_Process :: (Profunctor p, Functor f) => p (Process m a b) (f (Process m a b)) -> p (Object (Request a b) m) (f (Object (Request a b) m))
_Process = dimap Process (fmap unProcess)

instance Functor f => Functor (Process f a) where
  fmap f (Process o0) = Process $ go o0 where
    go o = Object $ \(Request a cont) -> fmap (cont *** go) $ runObject o (Request a f)

instance Applicative f => Applicative (Process f a) where
  pure a = Process go where
    go = Object $ \(Request _ cont) -> pure (cont a, go)
  Process f0 <*> Process a0 = Process $ go f0 a0 where
    go mf ma = Object $ \(Request a cont) -> (\(f, mf') (x, ma') -> (cont (f x), go mf' ma'))
      <$> runObject mf (Request a id)
      <*> runObject ma (Request a id)

instance (Applicative f, Monoid b) => Monoid (Process f a b) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Monad m => C.Category (Process m) where
  id = arr id
  Process g0 . Process f0 = Process $ go f0 g0 where
    go f g = Object $ \(Request a cont) -> runObject f (Request a id)
      >>= \(b, f') -> liftM (\(c, g') -> (cont c, go f' g')) $ runObject g (Request b id)

instance Monad m => Arrow (Process m) where
  arr f = Process go where
    go = Object $ \(Request a cont) -> return (cont (f a), go)
  first (Process f0) = Process $ go f0 where
    go f = Object $ \(Request (a, c) cont) -> liftM (\(b, f') -> (cont (b, c), go f')) $ runObject f (Request a id)
  second (Process f0) = Process $ go f0 where
    go f = Object $ \(Request (a, c) cont) -> liftM (\(d, f') -> (cont (a, d), go f')) $ runObject f (Request c id)

instance Monad m => ArrowChoice (Process m) where
  left (Process f0) = Process $ go f0 where
    go f = Object $ \(Request e cont) -> case e of
      Left a -> liftM (\(b, f') -> (cont (Left b), go f')) $ runObject f (Request a id)
      Right c -> return (cont (Right c), go f)
  right (Process f0) = Process $ go f0 where
    go f = Object $ \(Request e cont) -> case e of
      Right a -> liftM (\(b, f') -> (cont (Right b), go f')) $ runObject f (Request a id)
      Left c -> return (cont (Left c), go f)

instance Monad m => Profunctor (Process m) where
  dimap f g (Process f0) = Process (go f0) where
    go m = Object $ \(Request a cont) -> liftM (\(b, m') -> (cont (g b), go m')) $ runObject m (Request (f a) id)
  {-# INLINE dimap #-}

instance Monad m => Strong (Process m) where
  first' = first
  {-# INLINE first' #-}
  second' = second
  {-# INLINE second' #-}

instance Monad m => Choice (Process m) where
  left' = A.left
  {-# INLINE left' #-}
  right' = A.right
  {-# INLINE right' #-}

instance (Applicative m, Num o) => Num (Process m i o) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance (Applicative m, Fractional o) => Fractional (Process m i o) where
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  recip = fmap recip
  fromRational = pure . fromRational
