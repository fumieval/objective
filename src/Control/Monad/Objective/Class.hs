{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Objective.IO
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- 'MonadObjective' class and operations
--
-----------------------------------------------------------------------------
module Control.Monad.Objective.Class where
import Control.Object.Object
import Control.Elevator
import Control.Monad.Trans.State.Strict
import Control.Monad.Operational.Mini
import Data.Functor.Identity
import Control.Monad

type Inst' f g = Inst g f g

class Monad b => ObjectiveBase b where
  data Inst b (f :: * -> *) (g :: * -> *)
  type InstOf b o :: *
  type InstOf b (Object f g) = Inst b f g
  new :: Object f g -> b (Inst b f g)
  new = new
  invoke :: Monad m => (forall x. b x -> m x) -> (forall x. g x -> m x) -> Inst b f g -> f a -> m a

newSettle :: ObjectiveBase g => Object f g -> g (Inst g f g)
newSettle = new

type MonadObjective b m = (ObjectiveBase b, Elevate b m, Monad m)

(.->) :: (Monad m, ObjectiveBase m) => Inst m f m -> f a -> m a
(.->) = invoke id id
{-# INLINE (.->) #-}

(.-) :: (MonadObjective b m, Elevate g m) => Inst b f g -> f a -> m a
(.-) = invoke elevate elevate
{-# INLINE (.-) #-}

infixr 3 .-

-- | Invoke a method.
(.^) :: (MonadObjective b m, Elevate g m, Elevate e f) => Inst b f g -> e a -> m a
i .^ e = i .- elevate e
{-# INLINE (.^) #-}
infixr 3 .^

-- | (.^) for StateT
(.&) :: (MonadObjective b m, Elevate g m, Elevate (State s) f) => Inst b f g -> StateT s m a -> m a
i .& m = do
  s <- i .^ StateT (\s -> Identity (s, s))
  (a, s') <- runStateT m s
  i .^ StateT (\_ -> Identity (a, s'))

infixr 3 .&

(.!) :: (MonadObjective b m, Elevate g m) => Inst b f g -> Program f a -> m a
(.!) i = interpret (i.-)
{-# INLINE (.!) #-}

infixr 3 .!

-- | We can convert method invocation into an object trivially.
-- @invocation i = liftO (i.-)@
invocation :: (MonadObjective b m, Elevate g m) => Inst b f g -> Object f m
invocation i = Object $ liftM (\a -> (a, invocation i)). (i.-)
{-# INLINE invocation #-}
