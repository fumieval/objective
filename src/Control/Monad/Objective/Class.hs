{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
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

import Control.Monad
import Control.Elevator

type Instance' e m = Instance e m m

class Monad m => MonadObjective m where
  data Instance (e :: * -> *) (n :: * -> *) m
  -- | Send a message to the pointed one.
  invoke :: Monad n => Instance e n m -> e a -> m (n (m a))
  -- | Add an object to the environment.
  new :: Object e n -> m (Instance e n m)

(.-) :: (Monad n
  , Elevate n m
  , MonadObjective m
  , Elevate m f) => Instance e n m -> e a -> f a
a .- e = elevate $ invoke a e >>= join . elevate

infix 3 .-

(.^) :: (Elevate e f
  , Monad n
  , Elevate n m
  , MonadObjective m
  , Elevate m g) => Instance f n m -> e a -> g a
a .^ e = a .- elevate e

infix 3 .^