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

instance ObjectiveBase (ST s) where
  data Inst (ST s) f g = InstST (STRef s (Object f g))

  invoke mr gr (InstST ref) e = do
    o <- mr (readSTRef ref)
    (a, o') <- gr (runObject o e)
    mr (writeSTRef ref o')
    return a
  newBase o = InstST `fmap` newSTRef o
