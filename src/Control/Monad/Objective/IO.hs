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

instance ObjectiveBase IO where
  data Inst IO f g = InstIO (MVar (Object f g))

  invoke mr gr (InstIO m) e = do
    c <- mr (takeMVar m)
    (a, c') <- gr (runObject c e)
    mr (putMVar m c')
    return a

  newBase v = InstIO `fmap` newMVar v