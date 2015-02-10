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
    module Control.Object.Stream,
    module Control.Object.Mortal,
    module Control.Object.Extra,
    module Data.Functor.Request,
  ) where

import Control.Object.Object
import Control.Object.Stream
import Control.Object.Mortal
import Control.Object.Extra
import Data.Functor.Request
