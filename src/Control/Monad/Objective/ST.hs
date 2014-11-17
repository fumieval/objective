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
  data Instance e m (ST s) = InstanceST (STRef s (Object e m))

  InstanceST ref `invoke` e = do
      o <- readSTRef ref
      return $ do
        (a, o') <- runObject o e
        return $ writeSTRef ref o' >> return a
  new o = InstanceST `fmap` newSTRef o
