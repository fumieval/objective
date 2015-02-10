{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, ConstraintKinds, FlexibleContexts, TypeOperators, DataKinds, TypeFamilies #-}
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
import Control.Monad
import Data.Monoid
import Control.Applicative
import Data.Profunctor

-- | 'Request a b' is the type of a request that sends @a@ to receive @b@.
data Request a b r = Request a (b -> r) deriving (Functor, Typeable)

mapRequest :: (a -> a') -> Request a b r -> Request a' b r
mapRequest f (Request a br) = Request (f a) br
{-# INLINE mapRequest #-}

instance Profunctor (Request a) where
  dimap f g (Request a br) = Request a (dimap f g br)
  {-# INLINE dimap #-}

instance Monoid a => Applicative (Request a b) where
  pure a = Request mempty (const a)
  Request a c <*> Request b d = Request (mappend a b) (c <*> d)

request :: a -> Request a b b
request a = Request a id

accept :: Functor f => (a -> f b) -> Request a b r -> f r
accept f (Request a br) = fmap br (f a)

acceptM :: Monad m => (a -> m b) -> Request a b r -> m r
acceptM f (Request a br) = liftM br (f a)
