{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
module Control.Object.Mortal (
    Mortal(..),
    mortal,
    mortal_,
    runMortal,
    immortal,
    apprise
    ) where

import Control.Object.Object
import Control.Applicative
import Control.Monad.Trans.Either
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict
import Data.Monoid
import Data.Witherable
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
  m >>= k = mortal $ \f -> lift (runEitherT $ runMortal m f) >>= \r -> case r of
    Left a -> runMortal (k a) f
    Right (x, m') -> return (x, m' >>= k)

instance MonadTrans (Mortal f) where
  lift m = mortal $ const $ EitherT $ liftM Left m
  {-# INLINE lift #-}

-- | Construct a mortal in a 'Object' construction manner.
mortal :: (forall x. f x -> EitherT a m (x, Mortal f m a)) -> Mortal f m a
mortal f = Mortal (Object (fmap unsafeCoerce f))
{-# INLINE mortal #-}

-- | Send a message to a mortal.
runMortal :: Mortal f m a -> f x -> EitherT a m (x, Mortal f m a)
runMortal = unsafeCoerce
{-# INLINE runMortal #-}

-- | Restricted 'Mortal' constuctor which can be applied to 'transit', 'fromFoldable' without ambiguousness.
mortal_ :: Object f (EitherT () g) -> Mortal f g ()
mortal_ = Mortal
{-# INLINE mortal_ #-}

-- | Turn an immortal into a mortal with eternal life.
immortal :: Monad m => Object f m -> Mortal f m x
immortal obj = mortal $ \f -> EitherT $ runObject obj f >>= \(a, obj') -> return $ Right (a, immortal obj')

-- | Send a message to mortals in a container.
apprise :: (Witherable t, Monad m, Applicative m) => f a -> StateT (t (Mortal f m r)) m ([a], [r])
apprise f = StateT $ \t -> do
  (t', (Endo ba, Endo br)) <- runWriterT $ flip wither t
    $ \obj -> lift (runEitherT $ runMortal obj f) >>= \case
      Left r -> writer (Nothing, (mempty, Endo (r:)))
      Right (x, obj') -> writer (Just obj', (Endo (x:), mempty))
  return ((ba [], br []), t')
