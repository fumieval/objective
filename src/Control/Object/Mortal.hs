{-# LANGUAGE Rank2Types #-}
module Control.Object.Mortal (
    Mortal(..),
    mortal,
    mortal_,
    runMortal
    ) where

import Control.Object.Object
import Control.Applicative
import Control.Monad.Trans.Free
import Control.Monad.Trans.Either
import Control.Monad
import Control.Monad.Trans.Class
import Data.Profunctor.Unsafe
import Data.Void
import Unsafe.Coerce

-- | Object with a final result.
--
-- @Object f g ≡ Mortal f g Void@
--
newtype Mortal f g a = Mortal { unMortal :: Object f (EitherT a g) }

instance (Functor m, Monad m) => Functor (Mortal f m) where
  fmap f (Mortal obj) = Mortal (obj @>>^ bimapEitherT f id)

instance (Functor m, Monad m) => Applicative (Mortal f m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (Mortal f m) where
  return a = mortal $ const $ left a
  m >>= k = mortal $ \f -> lift (runMortal m f) >>= \r -> case r of
    Left a -> EitherT $ runMortal (k a) f
    Right (x, m') -> return (x, m' >>= k)

mortal :: (forall x. f x -> EitherT a m (x, Mortal f m a)) -> Mortal f m a
mortal f = unsafeCoerce f
{-# INLINE mortal #-}

runMortal :: Mortal f m a -> f x -> m (Either a (x, Mortal f m a))
runMortal = unsafeCoerce
{-# INLINE runMortal #-}

-- | Restricted 'Mortal' constuctor, which can be applied to 'transit', 'fromFoldable' without ambiguousness.
mortal_ :: Object f (EitherT () g) -> Mortal f g ()
mortal_ = Mortal
{-# INLINE mortal_ #-}
