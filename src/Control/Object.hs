{-# LANGUAGE Rank2Types, FlexibleInstances, FlexibleContexts, TypeOperators, CPP #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DeriveDataTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Object
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Stateful effect transducer: The Mealy machine for effects.
--
-----------------------------------------------------------------------------
module Control.Object (
  -- * Construction
  Object(..),
  liftO,
  echo,
  oneshot,
  stateful,
  variable,
  -- * Composition
  (.>>.),
  transObject,
  adaptObject,
  sequential,
  runSequential,
  -- * Multifunctional objects
  loner,
  (.|>.),
  sharing,
  -- * Patterns
  flyweight,
  Process(..),
  _Process
  )
where

import Control.Monad.Trans.State.Strict
import Control.Monad
import Data.Typeable
import Control.Applicative
import Data.OpenUnion1.Clean
import qualified Data.Map as Map
import Data.Functor.Request
import Control.Monad.Operational.Mini
import Control.Arrow
import qualified Control.Category as C
import Data.Profunctor
import Data.Monoid

-- | The type 'Object e m' represents objects which can handle messages @e@, perform actions in the environment @m@.
-- It can be thought of as an automaton that converts effects.
-- 'Object's can be composed just like functions using '.>>.'; the identity element is 'echo'.
newtype Object e m = Object { runObject :: forall x. e x -> m (x, Object e m) }
#if __GLASGOW_HASKELL__ >= 707
  deriving (Typeable)
#else
instance (Typeable1 f, Typeable1 m) => Typeable (Object f m) where
  typeOf t = mkTyConApp objectTyCon [typeOf1 (f t), typeOf1 (g t)] where
    f :: Object f m -> f a
    f = undefined
    g :: Object f m -> m a
    g = undefined

objectTyCon :: TyCon
#if __GLASGOW_HASKELL__ < 704
objectTyCon = mkTyCon "Control.Object.Object"
#else
objectTyCon = mkTyCon3 "object" "Control.Object" "Object"
#endif
{-# NOINLINE objectTyCon #-}
#endif

-- | Lift a natural transformation into an object.
liftO :: Functor f => (forall x. e x -> f x) -> Object e f
liftO f = Object $ fmap (\x -> (x, liftO f)) . f

-- | Change the workspace of the object.
transObject :: Functor g => (forall x. f x -> g x) -> Object e f -> Object e g
transObject f (Object m) = Object $ fmap (fmap (transObject f)) . f . m

-- | Apply a function to the messages coming into the object.
adaptObject :: Functor m => (forall x. e x -> f x) -> Object f m -> Object e m
adaptObject f (Object m) = Object $ fmap (fmap (adaptObject f)) . m . f

-- | Parrots messages given.
echo :: Functor e => Object e e
echo = Object (fmap (\x -> (x, echo)))

-- | Compose two objects (aka Dependency Injection).
(.>>.) :: Functor n => Object e m -> Object m n -> Object e n
Object m .>>. Object n = Object $ \e -> fmap (\((x, m'), n') -> (x, m' .>>. n')) $ n (m e)

infixr 4 .>>.

-- | Build an object using continuation passing style.
oneshot :: (Functor e, Monad m) => (forall a. e (m a) -> m a) -> Object e m
oneshot m = go where
  go = Object $ \e -> m (fmap return e) >>= \a -> return (a, go)
{-# INLINE oneshot #-}

-- | Build a stateful object.
stateful :: Monad m => (forall a. e a -> StateT s m a) -> s -> Object e m
stateful h = go where
  go s = Object $ liftM (\(a, s') -> (a, go s')) . flip runStateT s . h
{-# INLINE stateful #-}

-- | A mutable variable.
variable :: Applicative f => s -> Object (State s) f
variable s = Object $ \m -> let (a, s') = runState m s in pure (a, variable s')

-- | Build a stateful object, sharing out the state.
sharing :: Monad m => (forall a. e a -> StateT s m a) -> s -> Object (State s |> e |> Nil) m
sharing m = go where
  go s = Object $ \k -> liftM (fmap go) $ ($k)
    $ (\n -> return $ runState n s)
    ||> (\e -> runStateT (m e) s)
    ||> exhaust
{-# INLINE sharing #-}

-- | An object that won't accept any messages.
loner :: Functor m => Object Nil m
loner = liftO exhaust

-- | Extend an object by adding another independent object.
(.|>.) :: Functor m => Object f m -> Object (Union s) m -> Object (f |> Union s) m
p .|>. q = Object $ fmap (fmap (.|>.q)) . runObject p ||> fmap (fmap (p .|>.)) . runObject q

infixr 3 .|>.

-- | The flyweight pattern.
flyweight :: Monad m => Ord k => (k -> m a) -> Object (Request k a) m
flyweight f = go Map.empty where
  go m = Object $ \(Request k cont) -> case Map.lookup k m of
    Just a -> return (cont a, go m)
    Nothing -> f k >>= \a -> return (cont a, go $ Map.insert k a m)

runSequential :: Monad m => Object e m -> ReifiedProgram e a -> m (a, Object e m)
runSequential obj (Return a) = return (a, obj)
runSequential obj (e :>>= cont) = runObject obj e >>= \(a, obj') -> runSequential obj' (cont a)

-- | Let object handle sequential methods.
sequential :: Monad m => Object e m -> Object (ReifiedProgram e) m
sequential obj = Object $ liftM (fmap sequential) . runSequential obj

-- | An object which is specialized to be a Mealy machine
newtype Process m a b = Process { unProcess :: Object (Request a b) m }

-- | @_Process :: Iso' (Object (Request a b) m) (Process m a b)@
_Process :: (Profunctor p, Functor f) => p (Process m a b) (f (Process m a b)) -> (p (Object (Request a b) m) (f (Object (Request a b) m)))
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

instance Monad m => ArrowChoice (Process m) where
  left (Process f0) = Process $ go f0 where
    go f = Object $ \(Request e cont) -> case e of
      Left a -> liftM (\(b, f') -> (cont (Left b), go f')) $ runObject f (Request a id)
      Right c -> return (cont (Right c), go f)

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
