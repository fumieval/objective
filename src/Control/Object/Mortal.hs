{-# LANGUAGE Safe #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Object.Mortal
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  GADTs, Rank2Types
--
-----------------------------------------------------------------------------
module Control.Object.Mortal (
    Mortal(..),
    mortal,
    mortal_,
    runMortal,
    immortal,
    apprises,
    apprise
    ) where

import Control.Object.Object
import Control.Monad.Trans.Except
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict
import Data.Bifunctor
import Data.Monoid
import Witherable
import Data.Tuple (swap)

-- | A 'Mortal' is an object that may die.
-- A mortal yields a final result upon death.
-- @'Mortal' f g@ forms a 'Monad':
-- 'return' is a dead object and ('>>=') prolongs the life of the left object.
--
-- @Object f g ≡ Mortal f g Void@
--
newtype Mortal f g a = Mortal { unMortal :: Object f (ExceptT a g) }

instance Monad m => Functor (Mortal f m) where
  fmap f (Mortal obj) = Mortal (obj @>>^ mapExceptT (fmap (first f)))
  {-# INLINE fmap #-}

instance Monad m => Applicative (Mortal f m) where
  pure a = mortal $ const $ throwE a
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad m => Monad (Mortal f m) where
  m >>= k = mortal $ \f -> lift (runExceptT $ runMortal m f) >>= \case
    Left a -> runMortal (k a) f
    Right (x, m') -> return (x, m' >>= k)

instance MonadTrans (Mortal f) where
  lift m = mortal $ const $ ExceptT $ fmap Left m
  {-# INLINE lift #-}

-- | Construct a mortal in a 'Object' construction manner.
mortal :: Monad m => (forall x. f x -> ExceptT a m (x, Mortal f m a)) -> Mortal f m a
mortal f = Mortal (Object (fmap (fmap unMortal) . f))
{-# INLINE mortal #-}

-- | Send a message to a mortal.
runMortal :: Monad m => Mortal f m a -> f x -> ExceptT a m (x, Mortal f m a)
runMortal m f = fmap Mortal <$> runObject (unMortal m) f
{-# INLINE runMortal #-}

-- | A smart constructor of 'Mortal' where the result type is restricted to ()
mortal_ :: Object f (ExceptT () g) -> Mortal f g ()
mortal_ = Mortal
{-# INLINE mortal_ #-}

-- | Turn an object into a mortal without death.
immortal :: Monad m => Object f m -> Mortal f m x
immortal obj = Mortal (obj @>>^ lift)
{-# INLINE immortal #-}

-- | Send a message to mortals in a 'Witherable' container.
apprises :: (Witherable t, Monad m, Monoid r) => f a -> (a -> r) -> (b -> r) -> StateT (t (Mortal f m b)) m r
apprises f p q = StateT $ \t -> fmap swap $ runWriterT $ flip wither t
  $ \obj -> WriterT $ runExceptT (runMortal obj f) >>= \case
    Left r -> return (Nothing, q r)
    Right (x, obj') -> return (Just obj', p x)
{-# INLINE apprises #-}

-- | Send a message to mortals in a container.
apprise :: (Witherable t, Monad m) => f a -> StateT (t (Mortal f m r)) m ([a], [r])
apprise f = bimap (`appEndo` []) (`appEndo` [])
  <$> apprises f (\a -> (Endo (a:), mempty)) (\b -> (mempty, Endo (b:)))
{-# INLINE apprise #-}
