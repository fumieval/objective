{-# LANGUAGE Trustworthy #-}
module Control.Object.Stream where

import Data.Functor.Rep
import Data.Functor.Adjunction
import Control.Object.Object
import Data.Foldable as F
import Control.Applicative
import Data.Functor.Request
import Control.Monad
import Control.Monad.Trans.Either
import Control.Object.Mortal

-- | For every adjunction f ⊣ g, we can "connect" @Object g m@ and @Object f m@ permanently.
($$) :: (Monad m, Adjunction f g) => Object g m -> Object f m -> m x
a $$ b = do
  (x, a') <- runObject a askRep
  ((), b') <- runObject b (unit () `index` x)
  a' $$ b'
infix 1 $$

($?$) :: (Monad m, Adjunction f g) => Object g (EitherT a m) -> Object f (EitherT a m) -> m a
a $?$ b = liftM (either id id) $ runEitherT (a $$ b)
{-# INLINE ($?$) #-}

(!$$!) :: (Monad m, Adjunction f g) => Mortal g m a -> Mortal f m a -> m a
Mortal a !$$! Mortal b = a $?$ b
{-# INLINE (!$$!) #-}

-- | Create a source from a 'Foldable' container.
fromFoldable :: (Foldable t, Alternative m, Representable f) => t (Rep f) -> Object f m
fromFoldable = F.foldr go $ Object $ const empty where
  go x m = Object $ \cont -> pure (index cont x, m)

mapL :: (Adjunction f g, Adjunction f' g', Functor m) => (Rep g' -> Rep g) -> Object f m -> Object f' m
mapL t = (^>>@) $ rightAdjunct $ \x -> tabulate (index (unit x) . t)

mapR :: (Representable f, Representable g, Functor m) => (Rep f -> Rep g) -> Object f m -> Object g m
mapR t = (^>>@) $ \f -> tabulate (index f . t)

filterL :: (Adjunction f g, Applicative m) => (Rep g -> Bool) -> Object f m -> Object f m
filterL p obj = Object $ \f -> if counit (tabulate p <$ f)
  then fmap (filterL p) `fmap` runObject obj f
  else pure (extractL f, filterL p obj)

filterR :: (Representable f, Monad m) => (Rep f -> Bool) -> Object f m -> Object f m
filterR p obj = Object $ \f -> go f obj where
  go f o = do
    (x, o') <- runObject o askRep
    if p x
      then return (index f x, filterR p o')
      else go f o'

-- | Attack a rank-1 Mealy machine to a source.
($$@) :: (Representable f, Representable g, Monad m) => Object f m -> Object (Request (Rep f) (Rep g)) m -> Object g m
obj $$@ pro = Object $ \g -> do
  (x, obj') <- runObject obj askRep
  (a, pro') <- runObject pro $ Request x (index g)
  return (a, obj' $$@ pro')

-- | Attach a rank-1 Mealy machine into a sink.
(@$$) :: (Adjunction f g, Adjunction f' g', Monad m) => Object (Request (Rep g') (Rep g)) m -> Object f m -> Object f' m
pro @$$ obj = Object $ \f' -> do
  let (a, f_) = splitL f'
  (x, pro') <- runObject pro $ Request (counit (askRep <$ f_)) id
  ((), obj') <- runObject obj $ unit () `index` x
  return (a, pro' @$$ obj')
