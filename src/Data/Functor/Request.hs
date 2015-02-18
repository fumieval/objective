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
import Data.Monoid
import Control.Applicative
import Data.Profunctor
import Control.Object.Object
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Control.Arrow

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
{-# INLINE request #-}

handles :: Functor m => (a -> m (b, Object (Request a b) m)) -> Object (Request a b) m
handles f = Object $ \(Request a cont) -> first cont <$> f a
{-# INLINE handles #-}

-- | Like 'flyweight', but it uses 'Data.HashMap.Strict' internally.
flyweight :: (Applicative m, Eq k, Hashable k) => (k -> m a) -> Object (Request k a) m
flyweight f = go HM.empty where
  go m = Object $ \(Request k cont) -> case HM.lookup k m of
    Just a -> pure (cont a, go m)
    Nothing -> (\a -> (cont a, go $ HM.insert k a m)) <$> f k
{-# INLINE flyweight #-}

animate :: (Applicative m, Num t) => (t -> m a) -> Object (Request t a) m
animate f = go 0 where
  go t = Object $ \(Request dt cont) -> (\x -> (cont x, go (t + dt))) <$> f t
{-# INLINE animate #-}

transit :: (Alternative m, Fractional t, Ord t) => t -> (t -> m a) -> Object (Request t a) m
transit len f = animate go where
  go t
    | t >= len = empty
    | otherwise = f (t / len)
{-# INLINE transit #-}
