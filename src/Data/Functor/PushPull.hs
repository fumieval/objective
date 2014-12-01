{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, ConstraintKinds, FlexibleContexts, DataKinds, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.PushPull
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Data.Functor.PushPull where
import Data.Typeable
import Control.Elevator
import Data.OpenUnion1.Clean

-- | The type for asynchronous input/output.
data PushPull a b r = Push a r | Pull (b -> r) deriving (Functor, Typeable)

instance Tower (PushPull a b) where
  type Floors (PushPull a b) = Empty
  toLoft = exhaust

push :: (Elevate (PushPull a b) f) => a -> f ()
push a = elevate (Push a ())

pull :: (Elevate (PushPull a b) f) => f b
pull = elevate (Pull id)