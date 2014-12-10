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
import Control.Object
import Control.Elevator
import Control.Monad.Trans.State.Strict
import Control.Monad.Operational.Mini

type Inst' f g = Inst g f g

class ObjectiveBase b where
  data Inst b (f :: * -> *) (g :: * -> *)
  new :: Object f g -> b (Inst b f g)
  invoke :: Monad m => (forall x. b x -> m x) -> (forall x. g x -> m x) -> Inst b f g -> f a -> m a

(.-) :: (ObjectiveBase b, Elevate b m, Elevate g m, Monad m) => Inst b f g -> f a -> m a
(.-) = invoke elevate elevate
{-# INLINE (.-) #-}

infix 3 .-

-- | Invoke a method.
(.^) :: (ObjectiveBase b, Elevate b m, Elevate g m, Monad m, Elevate e f) => Inst b f g -> e a -> m a
i .^ e = i .- elevate e
{-# INLINE (.^) #-}
infix 3 .^

-- | Specialized (.^) for StateT
(.&) :: (ObjectiveBase b, Elevate b m, Elevate g m, Monad m, Elevate (State s) f) => Inst b f g -> StateT s m a -> m a
i .& m = do
  s <- i .^ get
  (a, s') <- runStateT m s
  i .^ put s'
  return a

infix 3 .&

(.!) :: (ObjectiveBase b, Elevate b m, Elevate g m, Monad m) => Inst b f g -> Program f a -> m a
(.!) i = interpret (i.-)

infix 3 .!
