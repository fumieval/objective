{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Objective.ST
-- Copyright   :  (c) Corbin Simpson, Google Inc. 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (ST)
--
-- 'MonadObjective' 'ST' using 'STRef'
--
-----------------------------------------------------------------------------
module Control.Monad.Objective.ST where

import Control.Monad.Objective.Class
import Control.Monad.ST
import Control.Object
import Data.STRef

instance MonadObjective (ST s) where
    type Residence (ST s) = ST s
    data Address e (ST s) = Address (STRef s (Object e (ST s)))

    Address ref .- e = do
        o <- readSTRef ref
        (a, o') <- runObject o e
        writeSTRef ref o'
        return a
    new o = Address `fmap` newSTRef o
