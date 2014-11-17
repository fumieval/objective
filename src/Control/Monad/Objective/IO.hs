{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import Control.Monad.Objective.Class
import Control.Concurrent
import Control.Object

instance MonadObjective IO where

  data Instance e m IO = InstanceIO (MVar (Object e m))

  InstanceIO m `invoke` e = do
    c <- takeMVar m
    return $ do
      (a, c') <- runObject c e
      return (putMVar m c' >> return a)
  new v = InstanceIO `fmap` newMVar v