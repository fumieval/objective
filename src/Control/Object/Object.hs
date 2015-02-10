{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE Rank2Types, CPP, TypeOperators, DataKinds, TupleSections #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DeriveDataTypeable #-}
#endif
module Control.Object.Object where
import Data.Typeable
import Control.Monad.Trans.State.Strict
import Control.Monad.Free
import Control.Monad

-- | The type @Object f g@ represents objects which can handle messages @f@, perform actions in the environment @g@.
-- It can be thought of as an automaton that converts effects.
-- 'Object's can be composed just like functions using '@>>@'; the identity element is 'echo'.
-- Objects are morphisms of the category of actions.
--
-- [/Naturality/]
--     @runObject obj . fmap f ≡ fmap f . runObject obj@
--
newtype Object f g = Object { runObject :: forall x. f x -> g (x, Object f g) }
  deriving (Typeable)

infixr 1 ^>>@
infixr 1 @>>^

class HProfunctor k where
  (^>>@) :: Functor h => (forall x. f x -> g x) -> k g h -> k f h
  (@>>^) :: Functor h => k f g -> (forall x. g x -> h x) -> k f h

instance HProfunctor Object where
  m0 @>>^ g = go m0 where go (Object m) = Object $ fmap (fmap go) . g . m
  f ^>>@ m0 = go m0 where go (Object m) = Object $ fmap (fmap go) . m . f

echo :: Functor f => Object f f
echo = Object $ fmap (,echo)

(@>>@) :: Functor h => Object f g -> Object g h -> Object f h
Object m @>>@ Object n = Object $ fmap (\((x, m'), n') -> (x, m' @>>@ n')) . n . m

liftO :: Functor g => (forall x. f x -> g x) -> Object f g
liftO f = go where go = Object $ fmap (\x -> (x, go)) . f

(@-) :: Object f g -> f x -> g (x, Object f g)
(@-) = runObject
{-# INLINE (@-) #-}
infixr 3 @-

unfoldO :: Functor g => (forall a. r -> f a -> g (a, r)) -> r -> Object f g
unfoldO h = go where go r = Object $ fmap (fmap go) . h r
{-# INLINE unfoldO #-}

-- | The unwrapped analog of 'stateful'
--     @unfoldO runObject = id@
--     @unfoldO iterObject = iterable@

unfoldOM :: Monad m => (forall a. r -> f a -> m (a, r)) -> r -> Object f m
unfoldOM h = go where go r = Object $ liftM (fmap go) . h r
{-# INLINE unfoldOM #-}

-- | Build a stateful object.
--
-- @stateful t s = t ^>>@ variable s@
stateful :: Monad m => (forall a. f a -> StateT s m a) -> s -> Object f m
stateful h = go where
  go s = Object $ \f -> runStateT (h f) s >>= \(a, s') -> s' `seq` return (a, go s')
{-# INLINE stateful #-}

infixr 1 @>>@

-- | Reversed '(@>>@)'
(@<<@) :: Functor h => Object g h -> Object f g -> Object f h
(@<<@) = flip (@>>@)
{-# INLINE (@<<@) #-}
infixl 1 @<<@

iterObject :: Monad m => Object f m -> Free f a -> m (a, Object f m)
iterObject obj (Pure a) = return (a, obj)
iterObject obj (Free f) = runObject obj f >>= \(cont, obj') -> iterObject obj' cont

iterative :: (Monad m) => Object f m -> Object (Free f) m
iterative = unfoldOM iterObject

