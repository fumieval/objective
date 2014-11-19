{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, ConstraintKinds, FlexibleContexts #-}
module Data.Functor.PushPull where
import Data.Typeable
import Control.Elevator

data PushPull a b r = Push a r | Pull (b -> r) deriving (Functor, Typeable)

instance Tower (PushPull a b)

push :: (Elevate (PushPull a b) f) => a -> f ()
push a = elevate (Push a ())

pull :: (Elevate (PushPull a b) f) => f b
pull = elevate (Pull id)