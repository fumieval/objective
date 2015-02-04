{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE Rank2Types #-}
module Control.Object.Mortal (
    Mortal(..),
    mortal,
    mortal_,
    runMortal,
    runMortal',
    immortal,
    reincarnation
    ) where

import Control.Object.Object
import Control.Applicative
import Control.Monad.Trans.Either
import Control.Monad
import Control.Monad.Trans.Class
import Unsafe.Coerce

-- | Object with a final result.
--
-- @Object f g ≡ Mortal f g Void@
--
newtype Mortal f g a = Mortal { unMortal :: Object f (EitherT a g) }

instance (Functor m, Monad m) => Functor (Mortal f m) where
  fmap f (Mortal obj) = Mortal (obj @>>^ bimapEitherT f id)
  {-# INLINE fmap #-}

instance (Functor m, Monad m) => Applicative (Mortal f m) where
  pure = return
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad m => Monad (Mortal f m) where
  return a = mortal $ const $ left a
  {-# INLINE return #-}
  m >>= k = mortal $ \f -> lift (runMortal' m f) >>= \r -> case r of
    Left a -> runMortal (k a) f
    Right (x, m') -> return (x, m' >>= k)

instance MonadTrans (Mortal f) where
  lift m = mortal $ const $ EitherT $ liftM Left m

mortal :: (forall x. f x -> EitherT a m (x, Mortal f m a)) -> Mortal f m a
mortal f = Mortal (Object (fmap unsafeCoerce f))
{-# INLINE mortal #-}

runMortal :: Mortal f m a -> f x -> EitherT a m (x, Mortal f m a)
runMortal = unsafeCoerce
{-# INLINE runMortal #-}

runMortal' :: Mortal f m a -> f x -> m (Either a (x, Mortal f m a))
runMortal' = unsafeCoerce
{-# INLINE runMortal' #-}

-- | Restricted 'Mortal' constuctor, which can be applied to 'transit', 'fromFoldable' without ambiguousness.
mortal_ :: Object f (EitherT () g) -> Mortal f g ()
mortal_ = Mortal
{-# INLINE mortal_ #-}

immortal :: Monad m => Object f m -> Mortal f m x
immortal obj = mortal $ \f -> EitherT $ runObject obj f >>= \(a, obj') -> return $ Right (a, immortal obj')

reincarnation :: Monad m => (a -> Mortal f m a) -> a -> Object f m
reincarnation g = go . g where
  go m = Object $ \f -> runMortal' m f >>= \r -> case r of
    Left a -> runObject (go (g a)) f
    Right (a, m') -> return (a, go m')
{-# INLINE reincarnation #-}
