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
import Data.Profunctor
import Data.Functor.Day

-- | The type for asynchronous input/output.
data PushPull a b r = Push a r | Pull (b -> r) deriving (Functor, Typeable)

type PushPull' a = PushPull a a

instance Profunctor (PushPull a) where
  dimap _ g (Push a r) = Push a (g r)
  dimap f g (Pull br) = Pull (dimap f g br)

instance Tower (PushPull a b) where
  type Floors (PushPull a b) = (,) a :> (->) b :> Empty
  toLoft = uncurry Push ||> Pull ||> exhaust

mapPush :: (a -> a') -> PushPull a b r -> PushPull a' b r
mapPush f (Push a r) = Push (f a) r
mapPush _ (Pull br) = Pull br

push :: (Elevate (PushPull a b) f) => a -> f ()
push a = elevate (Push a ())

pull :: (Elevate (PushPull a b) f) => f b
pull = elevate (Pull id)

bipush :: (i -> (a, c)) -> (b -> d -> o) -> PushPull i o r -> Day (PushPull a b) (PushPull c d) r
bipush f g = go where
  go (Pull r) = Day (Pull id) (Pull id) (fmap r . g)
  go (Push i r) = let (a, b) = f i in Day (Push a ()) (Push b ()) (\_ _ -> r)
{-# INLINE bipush #-}

bipull :: (a -> b -> c) -> PushPull i c r -> Day (PushPull i a) (PushPull i b) r
bipull = bipush (\x -> (x, x))
{-# INLINE bipull #-}

-- | @filterPush :: (a -> Bool) -> PushPull a b r -> Program (PushPull a b) r@
filterPush :: (Applicative f, Elevate (PushPull a b) f) => (a -> Bool) -> PushPull a b r -> f r
filterPush p e@(Push a r)
  | p a = elevate e
  | otherwise = pure r
filterPush _ e = elevate e
