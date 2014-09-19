{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances  #-}
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
-- 'MonadObjective' 'IO' using MVar
--
-----------------------------------------------------------------------------
module Control.Monad.Objective.IO  where
import GHC.Base
import Control.Monad.Objective.Class
import Control.Concurrent
import Control.Object

instance MonadObjective IO where

  type Residence IO = IO
  data Address e IO = Address (MVar (Object e IO))

  Address m .- e = do
      c <- takeMVar m
      (a, c') <- runObject c e
      putMVar m c'
      return a
  invoke v = do
      m <- newMVar v
      return (Address m)