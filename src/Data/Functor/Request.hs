{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, ConstraintKinds, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Request
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Data.Functor.Request where
import Data.Typeable
import Control.Elevator
import Control.Monad
import Data.Monoid
import Control.Applicative

-- | 'Request a b' is the type of a request that sends @a@ to receive @b@.
data Request a b r = Request a (b -> r) deriving (Functor, Typeable)

instance Monoid a => Applicative (Request a b) where
  pure a = Request mempty (const a)
  Request a c <*> Request b d = Request (mappend a b) (c <*> d)

instance Tower (Request a b)

request :: (Elevate (Request a b) f) => a -> f b
request a = elevate (Request a id)

accept :: Functor f => (a -> f b) -> Request a b r -> f r
accept f (Request a br) = fmap br (f a)

acceptM :: Monad m => (a -> m b) -> Request a b r -> m r
acceptM f (Request a br) = liftM br (f a)
