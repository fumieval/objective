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
  -- * Multifunctional objects
  loner,
  (.|>.),
  sharing,
  -- * Patterns
  flyweight
  )
where

import Control.Monad.Trans.State.Strict
import Control.Monad
import Data.Typeable
import Control.Applicative
import Data.OpenUnion1.Clean
import qualified Data.Map as Map
import Data.Functor.Request

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

