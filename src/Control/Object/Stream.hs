module Control.Object.Stream where

import Data.Functor.Rep
import Data.Functor.Adjunction
import Control.Object.Object
import Data.Foldable as F
import Control.Object.Mortal
import Control.Applicative

-- | For every adjunction f ⊣ g, we can "connect" @Object g m@ and @Object f m@ permanently.
($$) :: (Monad m, Adjunction f g) => Object g m -> Object f m -> m x
a $$ b = do
  (x, a') <- runObject a askRep
  ((), b') <- runObject b (unit () `index` x)
  a' $$ b'
infix 1 $$

-- | Like '$$', but kept until the right 'Mortal' dies.
($$!) :: (Monad m, Adjunction f g) => Object g m -> Mortal f m a -> m (Object g m, a)
o $$! m = do
  (x, o') <- runObject o askRep
  r <- runMortal m (unit () `index` x)
  case r of
    Left a -> return (o', a)
    Right ((), m') -> o' $$! m'
infix 1 $$!

-- | Like '$$', but kept until the left 'Mortal' dies.
(!$$) :: (Monad m, Adjunction f g) => Mortal g m a -> Object f m -> m (a, Object f m)
m !$$ o = do
  r <- runMortal m askRep
  case r of
    Left a -> return (a, o)
    Right (x, m') -> do
      ((), o') <- runObject o (unit () `index` x)
      m' !$$ o'
infix 1 !$$

-- | Connect two 'Mortal's.
(!$$!) :: (Monad m, Adjunction f g) => Mortal g m a -> Mortal f m b -> m (Either (a, Mortal f m b) (Mortal g m a, b))
m !$$! n = do
  r <- runMortal m askRep
  case r of
    Left a -> return (Left (a, n))
    Right (x, m') -> do
      s <- runMortal n (unit () `index` x)
      case s of
        Left b -> return (Right (m', b))
        Right ((), n') -> m' !$$! n'

infix 1 !$$!

-- | 'filter' for consumers.
filterL :: (Adjunction f g, Applicative m) => (Rep g -> Bool) -> Object f m -> Object f m
filterL p obj = Object $ \f -> if counit (tabulate p <$ f)
  then fmap (filterL p) `fmap` runObject obj f
  else pure (extractL f, filterL p obj)

-- | Create a producer from a 'Foldable' container.
fromFoldable :: (Foldable t, Alternative m, Adjunction f g) => t (Rep g) -> Object g m
fromFoldable = F.foldr go $ Object $ const empty where
  go x m = Object $ \cont -> pure (index cont x, m)
