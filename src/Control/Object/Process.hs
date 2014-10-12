module Control.Object.Process where
import Control.Object
import Control.Arrow
import Control.Applicative
import qualified Control.Category as C
import Data.Profunctor
import Data.Monoid
import Control.Monad

-- Object specialized to a one-to-one mealy machine
newtype Process m a b = Process { unProcess :: Object (Request a b) m }

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
      >>= \(b, f') -> liftM (\(c, g') -> (cont c, go f' g')) $ runObject f (Request b id)

instance Monad m => Arrow (Process m) where
  arr f = Process go where
    go = Object $ \(Request a cont) -> return (cont (f a), go)
  first (Process f0) = Process $ go f0 where
    go f = Object $ \(Request (a, c) cont) -> liftM (\(b, f') -> ((b, c), go f')) $ runObject f (Request a id)

instance Monad m => ArrowChoice (Process f) where
  left (Process f0) = Process $ go f0 where
    go f = Object $ \(Request e cont) -> case e of
      Right c -> return (c, go f)
{-
instance Applicative m => Profunctor (Process m) where
  dimap f g = go where
    go (Process v) = Process $ \i cont -> v (f i) $ \o v' -> cont (g o) (go v')
  {-# INLINE dimap #-}
-}
instance Applicative m => Strong (Process m) where
  first' = first
  {-# INLINE first' #-}
  second' = second
  {-# INLINE second' #-}

instance Applicative m => Choice (Process m) where
  left' = left
  {-# INLINE left' #-}
  right' = right 
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