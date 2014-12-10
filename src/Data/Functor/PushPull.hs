{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, ConstraintKinds, FlexibleContexts, DataKinds, TypeFamilies, TypeOperators #-}
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
import Control.Applicative

-- | The type for asynchronous input/output.
data PushPull a b r = Push a r | Pull (b -> r) deriving (Functor, Typeable)

instance Tower (PushPull a b) where
  type Floors (PushPull a b) = (,) a :> (->) b :> Empty
  toLoft = uncurry Push ||> Pull ||> exhaust

push :: (Elevate (PushPull a b) f) => a -> f ()
push a = elevate (Push a ())

pull :: (Elevate (PushPull a b) f) => f b
pull = elevate (Pull id)

-- | @filterPush :: (a -> Bool) -> PushPull a b r -> Program (PushPull a b) r@
filterPush :: (Applicative f, Elevate (PushPull a b) f) => (a -> Bool) -> PushPull a b r -> f r
filterPush p e@(Push a r)
  | p a = elevate e
  | otherwise = pure r
filterPush _ e = elevate e
