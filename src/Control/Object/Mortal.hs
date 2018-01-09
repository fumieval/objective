{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
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
    apprisesOf,
    apprises,
    apprise
    ) where

import Control.Object.Object
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Monad.Trans.Except
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict
import Data.Bifunctor
import Data.Monoid
import Data.Witherable
import Data.Tuple (swap)
import Control.Arrow ((***))
import Unsafe.Coerce

-- | A 'Mortal' is an object that may die.
-- A mortal yields a final result upon death.
-- @'Mortal' f g@ forms a 'Monad':
-- 'return' is a dead object and ('>>=') prolongs the life of the left object.
--
-- @Object f g ≡ Mortal f g Void@
--
newtype Mortal f g a = Mortal { unMortal :: Object f (ExceptT a g) }

instance (Functor m, Monad m) => Functor (Mortal f m) where
  fmap f (Mortal obj) = Mortal (obj @>>^ mapExceptT (fmap (first f)))
  {-# INLINE fmap #-}

instance (Functor m, Monad m) => Applicative (Mortal f m) where
  pure = return
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad m => Monad (Mortal f m) where
  return a = mortal $ const $ throwE a
  {-# INLINE return #-}
  m >>= k = mortal $ \f -> lift (runExceptT $ runMortal m f) >>= \r -> case r of
    Left a -> runMortal (k a) f
    Right (x, m') -> return (x, m' >>= k)

instance MonadTrans (Mortal f) where
  lift m = mortal $ const $ ExceptT $ liftM Left m
  {-# INLINE lift #-}

-- | Construct a mortal in a 'Object' construction manner.
mortal :: Monad m => (forall x. f x -> ExceptT a m (x, Mortal f m a)) -> Mortal f m a
mortal f = unsafeCoerce f `asTypeOf` Mortal (Object (liftM (fmap unMortal) . f))
{-# INLINE mortal #-}

-- | Send a message to a mortal.
runMortal :: Monad m => Mortal f m a -> f x -> ExceptT a m (x, Mortal f m a)
runMortal = unsafeCoerce `asTypeOf` ((liftM (fmap Mortal) . ) . runObject . unMortal)
{-# INLINE runMortal #-}

-- | Restricted 'Mortal' constuctor which can be applied to 'transit', 'fromFoldable' without ambiguousness.
mortal_ :: Object f (ExceptT () g) -> Mortal f g ()
mortal_ = Mortal
{-# INLINE mortal_ #-}

-- | Turn an object into a mortal without death.
immortal :: Monad m => Object f m -> Mortal f m x
immortal obj = Mortal (obj @>>^ lift)
{-# INLINE immortal #-}

-- | Send a message to mortals through a filter.
apprisesOf :: Monad m
  => FilterLike' (WriterT r m) s (Mortal f m b)
  -> f a -> (a -> r) -> (b -> r) -> StateT s m r
apprisesOf l f p q = StateT $ \t -> liftM swap $ runWriterT $ flip l t
    $ \obj -> WriterT $ runExceptT (runMortal obj f) >>= \case
      Left r -> return (Nothing, q r)
      Right (x, obj') -> return (Just obj', p x)
{-# INLINABLE apprisesOf #-}

-- | Send a message to mortals in a 'Witherable' container.
--
-- @apprises = apprisesOf wither@
--
apprises :: (Witherable t, Monad m, Applicative m, Monoid r) => f a -> (a -> r) -> (b -> r) -> StateT (t (Mortal f m b)) m r
apprises = apprisesOf wither
{-# INLINE apprises #-}

-- | Send a message to mortals in a container.
apprise :: (Witherable t, Monad m, Applicative m) => f a -> StateT (t (Mortal f m r)) m ([a], [r])
apprise f = fmap (flip appEndo [] *** flip appEndo [])
  $ apprises f (\a -> (Endo (a:), mempty)) (\b -> (mempty, Endo (b:)))
{-# INLINE apprise #-}
