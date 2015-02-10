{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE Rank2Types, FlexibleInstances, FlexibleContexts, TypeOperators, CPP, ConstraintKinds #-}

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
module Control.Object
  ( module Control.Object.Object,
    module Control.Object.Mortal,
    module Control.Object.Suited,
    module Control.Object.Instance,
    module Data.Functor.Request,
  ) where

import Control.Object.Object
import Control.Object.Mortal
import Control.Object.Instance
import Control.Object.Suited
import Data.Functor.Request
