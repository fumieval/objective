{-# LANGUAGE RankNTypes, TupleSections, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Object.Object
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  GADTs, Rank2Types
--
-----------------------------------------------------------------------------
module Control.Object.Object (Object(..)
  , echo
  , (@>>@)
  , (@<<@)
  , liftO
  , (^>>@)
  , (@>>^)
  , (@||@)
  -- * Stateful construction
  , unfoldO
  , unfoldOM
  , stateful
  , (@~)
  , variable
  -- * Method cascading
  , (@-)
  , iterObject
  , iterative
  , cascadeObject
  , cascading
  -- * Filtering
  , Fallible(..)
  , filteredO
  , filterO
  -- * Manipulation on StateT
  , invokesOf
  , invokes
  , (@!=)
  , announce
  , withBuilder
  ) where
import Control.Monad.Trans.State.Strict
import Control.Monad.Free
import Control.Monad
import Control.Monad.Skeleton
import Data.Traversable as T
import Control.Monad.Trans.Writer.Strict
import Data.Monoid
import Data.Tuple (swap)
import qualified Data.Functor.Sum as Functor

-- | The type @Object f g@ represents objects which can handle messages @f@, perform actions in the environment @g@.
-- It can be thought of as an automaton that transforms effects.
-- 'Object's can be composed just like functions using '@>>@'; the identity element is 'echo'.
-- Objects are morphisms of the category of actions.
--
-- [/Naturality/]
--     @runObject obj . fmap f ≡ fmap f . runObject obj@
--
newtype Object f g = Object { runObject :: forall x. f x -> g (x, Object f g) }

-- | An infix alias for 'runObject'
(@-) :: Object f g -> f x -> g (x, Object f g)
(@-) = runObject
{-# INLINE (@-) #-}
infixr 3 @-

infixr 1 ^>>@
infixr 1 @>>^

(^>>@) :: Functor h => (forall x. f x -> g x) -> Object g h -> Object f h
f ^>>@ m0 = go m0 where go (Object m) = Object $ fmap (fmap go) . m . f
{-# INLINE (^>>@) #-}

(@>>^) :: Functor h => Object f g -> (forall x. g x -> h x) -> Object f h
m0 @>>^ g = go m0 where go (Object m) = Object $ fmap (fmap go) . g . m
{-# INLINE (@>>^) #-}

-- | The trivial object
echo :: Functor f => Object f f
echo = Object $ fmap (,echo)

-- | Lift a natural transformation into an object.
liftO :: Functor g => (forall x. f x -> g x) -> Object f g
liftO f = go where go = Object $ fmap (\x -> (x, go)) . f
{-# INLINE liftO #-}

-- | The categorical composition of objects.
(@>>@) :: Functor h => Object f g -> Object g h -> Object f h
Object m @>>@ Object n = Object $ fmap (\((x, m'), n') -> (x, m' @>>@ n')) . n . m
infixr 1 @>>@

-- | Reversed '(@>>@)'
(@<<@) :: Functor h => Object g h -> Object f g -> Object f h
(@<<@) = flip (@>>@)
{-# INLINE (@<<@) #-}
infixl 1 @<<@

-- | Combine objects so as to handle a 'Functor.Sum' of interfaces.
(@||@) :: Functor h => Object f h -> Object g h -> Object (f `Functor.Sum` g) h
a @||@ b = Object $ \r -> case r of
  Functor.InL f -> fmap (fmap (@||@b)) (runObject a f)
  Functor.InR g -> fmap (fmap (a@||@)) (runObject b g)

-- | An unwrapped analog of 'stateful'
--     @id = unfoldO runObject@
--     @'iterative' = unfoldO 'iterObject'@
--     @'cascading' = unfoldO 'cascadeObject'@
unfoldO :: Functor g => (forall a. r -> f a -> g (a, r)) -> r -> Object f g
unfoldO h = go where go r = Object $ fmap (fmap go) . h r
{-# INLINE unfoldO #-}

-- | Same as 'unfoldO' but requires 'Monad' instead
unfoldOM :: Monad m => (forall a. r -> f a -> m (a, r)) -> r -> Object f m
unfoldOM h = go where go r = Object $ liftM (fmap go) . h r
{-# INLINE unfoldOM #-}

-- | Build a stateful object.
--
-- @stateful t s = t ^>>\@ variable s@
--
stateful :: Monad m => (forall a. t a -> StateT s m a) -> s -> Object t m
stateful h = go where
  go s = Object $ \f -> runStateT (h f) s >>= \(a, s') -> s' `seq` return (a, go s')
{-# INLINE stateful #-}

-- | Flipped 'stateful'.
-- it is super convenient to use with the LambdaCase extension.
(@~) :: Monad m => s -> (forall a. t a -> StateT s m a) -> Object t m
s @~ h = stateful h s
{-# INLINE (@~) #-}
infix 1 @~

-- | Cascading
iterObject :: Monad m => Object f m -> Free f a -> m (a, Object f m)
iterObject obj (Pure a) = return (a, obj)
iterObject obj (Free f) = runObject obj f >>= \(cont, obj') -> iterObject obj' cont

-- | Objects can consume free monads. 'cascading' is more preferred.
iterative :: Monad m => Object f m -> Object (Free f) m
iterative = unfoldOM iterObject
{-# INLINE iterative #-}

-- | A mutable variable.
--
-- @variable = stateful id@
--
variable :: Monad m => s -> Object (StateT s m) m
variable = stateful id
{-# INLINE variable #-}

-- | Pass zero or more messages to an object.
cascadeObject :: Monad m => Object t m -> Skeleton t a -> m (a, Object t m)
cascadeObject obj sk = case debone sk of
  Return a -> return (a, obj)
  t :>>= k -> runObject obj t >>= \(a, obj') -> cascadeObject obj' (k a)

-- | Add capability to handle multiple messages at once.
cascading :: Monad m => Object t m -> Object (Skeleton t) m
cascading = unfoldOM cascadeObject
{-# INLINE cascading #-}

-- | Send a message to an object through a lens.
invokesOf :: Monad m
  => ((Object t m -> WriterT r m (Object t m)) -> s -> WriterT r m s)
  -> t a -> (a -> r) -> StateT s m r
invokesOf t f c = StateT $ liftM swap . runWriterT
  . t (\obj -> WriterT $ runObject obj f >>= \(x, obj') -> return (obj', c x))
{-# INLINABLE invokesOf #-}

invokes :: (T.Traversable t, Monad m, Monoid r)
  => f a -> (a -> r) -> StateT (t (Object f m)) m r
invokes = invokesOf T.mapM
{-# INLINE invokes #-}

-- | Send a message to objects in a traversable container.
--
-- @announce = withBuilder . invokesOf traverse@
--
announce :: (T.Traversable t, Monad m) => f a -> StateT (t (Object f m)) m [a]
announce f = withBuilderM (invokes f)
{-# INLINABLE announce #-}

-- | A method invocation operator on 'StateT'.
(@!=) :: Monad m
  => ((Object t m -> WriterT a m (Object t m)) -> s -> WriterT a m s)
  -> t a -> StateT s m a
l @!= f = invokesOf l f id
{-# INLINE (@!=) #-}

withBuilder :: Functor f => ((a -> Endo [a]) -> f (Endo [a])) -> f [a]
withBuilder f = fmap (flip appEndo []) (f (Endo . (:)))
{-# INLINABLE withBuilder #-}

withBuilderM :: Monad f => ((a -> Endo [a]) -> f (Endo [a])) -> f [a]
withBuilderM f = liftM (flip appEndo []) (f (Endo . (:)))
{-# INLINABLE withBuilderM #-}

data Fallible t a where
  Fallible :: t a -> Fallible t (Maybe a)

filteredO :: Monad m
       => (forall x. t x -> Bool)
       -> Object t m -> Object (Fallible t) m
filteredO p obj = Object $ \(Fallible t) -> if p t
  then runObject obj t >>= \(a, obj') -> return (Just a, filteredO p obj')
  else return (Nothing, filteredO p obj)

filterO :: (forall x. t x -> Bool) -> Object (Fallible t) (Skeleton t)
filterO p = filteredO p (liftO bone)
{-# INLINE filterO #-}
