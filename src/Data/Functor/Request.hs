{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, ConstraintKinds, FlexibleContexts #-}
module Data.Functor.Request where
import Data.Typeable
import Control.Elevator
import Control.Monad

data Request a b r = Request a (b -> r) deriving (Functor, Typeable)

instance Tower (Request a b)

request :: (Elevate (Request a b) f) => a -> f b
request a = elevate (Request a id)

accept :: Functor f => (a -> f b) -> Request a b r -> f r
accept f (Request a br) = fmap br (f a)

acceptM :: Monad m => (a -> m b) -> Request a b r -> m r
acceptM f (Request a br) = liftM br (f a)
