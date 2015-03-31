{-# LANGUAGE CPP, DeriveFunctor, DeriveDataTypeable, ConstraintKinds, FlexibleContexts, TypeOperators, DataKinds, TypeFamilies #-}
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
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif
import Control.Applicative
import Data.Profunctor
import Control.Object.Object
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Control.Arrow

-- | @'Request' a b@ is the type of a request that sends @a@ to receive @b@.
data Request a b r = Request a (b -> r) deriving (Functor, Typeable)

mapRequest :: (a -> a') -> Request a b r -> Request a' b r
mapRequest f (Request a br) = Request (f a) br
{-# INLINE mapRequest #-}

instance Profunctor (Request a) where
  dimap f g (Request a br) = Request a (dimap f g br)
  {-# INLINE dimap #-}

instance Monoid a => Applicative (Request a b) where
  pure a = Request mempty (const a)
  {-# INLINE pure #-}
  Request a c <*> Request b d = Request (mappend a b) (c <*> d)
  {-# INLINE (<*>) #-}

request :: a -> Request a b b
request a = Request a id
{-# INLINE request #-}

accept :: Functor m => (a -> m b) -> Request a b r -> m r
accept f = \(Request a cont) -> cont <$> f a
{-# INLINE accept #-}

mealy :: Functor m => (a -> m (b, Object (Request a b) m)) -> Object (Request a b) m
mealy f = Object $ \(Request a cont) -> first cont <$> f a
{-# INLINE mealy #-}

-- | The flyweight pattern
flyweight :: (Applicative m, Eq k, Hashable k) => (k -> m a) -> Object (Request k a) m
flyweight f = go HM.empty where
  go m = mealy $ \k -> case HM.lookup k m of
    Just a -> pure (a, go m)
    Nothing -> (\a -> (a, go $ HM.insert k a m)) <$> f k
{-# INLINE flyweight #-}

(>~~>) :: Monad m => Object (Request a b) m -> Object (Request b c) m -> Object (Request a c) m
p >~~> q = Object $ \(Request a cont) -> do
  (b, p') <- runObject p (Request a id)
  (r, q') <- runObject q (Request b cont)
  return (r, p' >~~> q')

accumulator :: Applicative m => (b -> a -> b) -> b -> Object (Request a b) m
accumulator f = go where
  go b = mealy $ \a -> pure (b, go (f b a))
{-# INLINE accumulator #-}

-- |
-- @
-- animate f ≡ accumulator (+) 0 >~~> liftO (accept f)
-- @
animate :: (Applicative m, Num t) => (t -> m a) -> Object (Request t a) m
animate f = go 0 where
  go t = mealy $ \dt -> flip (,) (go (t + dt)) <$> f t
{-# INLINE animate #-}

transit :: (Alternative m, Fractional t, Ord t) => t -> (t -> m a) -> Object (Request t a) m
transit len f = animate go where
  go t
    | t >= len = empty
    | otherwise = f (t / len)
{-# INLINE transit #-}
