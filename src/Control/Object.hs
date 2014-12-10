{-# LANGUAGE Rank2Types, FlexibleInstances, FlexibleContexts, TypeOperators, CPP, ConstraintKinds #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DeriveDataTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Object
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Stateful effect transducer: The Mealy machine for effects.
--
-----------------------------------------------------------------------------
module Control.Object (
  -- * Construction
  Object(..),
  liftO,
  echo,
  oneshot,
  stateful,
  variable,
  foldO,
  foldOM,
  -- * Composition
  (@>>@),
  (@>>^),
  (^>>@),
  transObject,
  adaptObject,
  -- * Monads
  (@!),
  (@!!),
  sequential,
  sequentialT,
  iterObject,
  iterTObject,
  iterative,
  iterativeT,
  -- * Multifunctional objects
  loner,
  (@|>@),
  sharing,
  -- * Patterns
  flyweight,
  announce,
  announceMaybe,
  announceMaybeT,
  Process(..),
  _Process,
  -- * Deprecated
  runSequential
  )
where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Operational.Mini
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Class
import Control.Elevator
import Data.Functor.Request
import Data.Monoid
import Data.OpenUnion1.Clean
import Data.Profunctor
import Data.Typeable
import Data.Witherable
import qualified Control.Category as C
import qualified Control.Monad.Trans.Operational.Mini as T
import qualified Control.Monad.Trans.Free as T
import qualified Data.Map.Strict as Map
import qualified Data.Traversable as T
import Control.Monad.Free

-- | The type 'Object f g' represents objects which can handle messages @f@, perform actions in the environment @g@.
-- It can be thought of as an automaton that converts effects.
-- 'Object's can be composed just like functions using '@>>@'; the identity element is 'echo'.
newtype Object f g = Object { runObject :: forall x. f x -> g (x, Object f g) }
#if __GLASGOW_HASKELL__ >= 707
  deriving (Typeable)
#else
instance (Typeable1 f, Typeable1 g) => Typeable (Object f g) where
  typeOf t = mkTyConApp objectTyCon [typeOf1 (f t), typeOf1 (g t)] where
    f :: Object f g -> f a
    f = undefined
    g :: Object f g -> g a
    g = undefined

objectTyCon :: TyCon
#if __GLASGOW_HASKELL__ < 704
objectTyCon = mkTyCon "Control.Object.Object"
#else
objectTyCon = mkTyCon3 "object" "Control.Object" "Object"
#endif
{-# NOINLINE objectTyCon #-}
#endif

echo :: Functor f => Object f f
echo = Object (fmap (\x -> (x, echo)))

(@>>@) :: Functor h => Object f g -> Object g h -> Object f h
Object m @>>@ Object n = Object $ \e -> fmap (\((x, m'), n') -> (x, m' @>>@ n')) $ n (m e)

(@>>^) :: Functor h => Object f g -> (forall x. g x -> h x) -> Object f h
m0 @>>^ g = go m0 where go (Object m) = Object $ fmap (fmap go) . g . m

(^>>@) :: Functor h => (forall x. f x -> g x) -> Object g h -> Object f h
f ^>>@ m0 = go m0 where go (Object m) = Object $ fmap (fmap go) . m . f

-- | Lift a natural transformation into an object.
liftO :: Functor g => (forall x. f x -> g x) -> Object f g
liftO f = go where go = Object $ fmap (\x -> (x, go)) . f
{-# INLINE liftO #-}

-- | Change the workspace of the object.
transObject :: Functor g => (forall x. f x -> g x) -> Object e f -> Object e g
transObject f = (@>>^f)

-- | Apply a function to the messages coming into the object.
adaptObject :: Functor m => (forall x. g x -> f x) -> Object f m -> Object g m
adaptObject f = (f^>>@)

-- | Build an object using continuation passing style.
oneshot :: (Functor f, Monad m) => (forall a. f (m a) -> m a) -> Object f m
oneshot m = go where
  go = Object $ \e -> m (fmap return e) >>= \a -> return (a, go)
{-# INLINE oneshot #-}

-- | Build a stateful object.
-- @stateful t s = t ^>>> variable s@
stateful :: Monad m => (forall a. f a -> StateT s m a) -> s -> Object f m
stateful h = go where
  go s = Object $ liftM (\(a, s') -> (a, go s')) . flip runStateT s . h
{-# INLINE stateful #-}

-- | fold-like, unwrapped analog of 'stateful'
--     @folder runObject = id@
--     @folder runSequential = sequential@
--     @folder iterObject = iterable@
foldO :: Functor g => (forall a. r -> f a -> g (a, r)) -> r -> Object f g
foldO h = go where go r = Object $ fmap (fmap go) . h r
{-# INLINE foldO #-}

foldOM :: Monad m => (forall a. r -> f a -> m (a, r)) -> r -> Object f m
foldOM h = go where go r = Object $ liftM (fmap go) . h r
{-# INLINE foldOM #-}

-- | A mutable variable.
variable :: Monad m => s -> Object (StateT s m) m
variable s = Object $ \m -> liftM (fmap variable) $ runStateT m s

-- | Build a stateful object, sharing out the state.
sharing :: Monad m => (forall a. f a -> StateT s m a) -> s -> Object (State s |> f |> Nil) m
sharing m = go where
  go s = Object $ \k -> liftM (fmap go) $ ($k)
    $ (\n -> return $ runState n s)
    ||> (\e -> runStateT (m e) s)
    ||> exhaust
{-# INLINE sharing #-}

-- | An object that won't accept any messages.
loner :: Functor f => Object Nil f
loner = liftO exhaust

-- | Extend an object by adding another independent object.
(@|>@) :: Functor g => Object f g -> Object (Union s) g -> Object (f |> Union s) g
p @|>@ q = Object $ fmap (fmap (@|>@q)) . runObject p ||> fmap (fmap (p @|>@)) . runObject q
infixr 3 @|>@

-- | The flyweight pattern.
flyweight :: Monad m => Ord k => (k -> m a) -> Object (Request k a) m
flyweight f = go Map.empty where
  go m = Object $ \(Request k cont) -> case Map.lookup k m of
    Just a -> return (cont a, go m)
    Nothing -> f k >>= \a -> return (cont a, go $ Map.insert k a m)

(@!) :: Monad m => Object e m -> ReifiedProgram e a -> m (a, Object e m)
obj @! Return a = return (a, obj)
obj @! (e :>>= cont) = runObject obj e >>= \(a, obj') -> obj' @! cont a

(@!!) :: Monad m => Object e m -> T.ReifiedProgramT e m a -> m (a, Object e m)
obj @!! T.Return a = return (a, obj)
obj @!! T.Lift m cont = m >>= (obj @!!) . cont
obj @!! (e T.:>>= cont) = runObject obj e >>= \(a, obj') -> obj' @!! cont a

runSequential :: Monad m => Object e m -> ReifiedProgram e a -> m (a, Object e m)
runSequential = (@!)
{-# DEPRECATED runSequential "use (@!!) instead" #-}

iterObject :: Monad m => Object f m -> Free f a -> m (a, Object f m)
iterObject obj (Pure a) = return (a, obj)
iterObject obj (Free f) = runObject obj f >>= \(cont, obj') -> iterObject obj' cont

iterTObject :: Monad m => Object f m -> T.FreeT f m a -> m (a, Object f m)
iterTObject obj m = T.runFreeT m >>= \r -> case r of
  T.Pure a -> return (a, obj)
  T.Free f -> runObject obj f >>= \(cont, obj') -> iterTObject obj' cont

-- | Let object handle sequential methods.
sequential :: Monad m => Object e m -> Object (ReifiedProgram e) m
sequential = foldOM (@!)

-- | Let object handle sequential methods.
sequentialT :: Monad m => Object e m -> Object (T.ReifiedProgramT e m) m
sequentialT = foldOM (@!!)

iterative :: Monad m => Object f m -> Object (Free f) m
iterative = foldOM iterObject

iterativeT :: Monad m => Object f m -> Object (T.FreeT f m) m
iterativeT = foldOM iterTObject

announce :: (T.Traversable t, Monad m, Elevate (State (t (Object f g))) m, Elevate g m) => f a -> m [a]
announce f = do
  t <- elevate get
  (t', Endo e) <- runWriterT $ T.mapM (\obj -> (lift . elevate) (runObject obj f)
      >>= \(x, obj') -> writer (obj', Endo (x:))) t
  elevate (put t')
  return (e [])

announceMaybe :: (Witherable t, Monad m, Elevate (State (t (Object f Maybe))) m) => f a -> m [a]
announceMaybe f = elevate $ state
  $ \t -> let (t', Endo e) = runWriter
                $ witherM (\obj -> case runObject obj f of
                  Just (x, obj') -> lift $ writer (obj', Endo (x:))
                  Nothing -> mzero) t in (e [], t')

announceMaybeT :: (Witherable t, Monad m, State (t (Object f (MaybeT g))) ∈ Floors1 m, g ∈ Floors1 m, Tower m) => f a -> m [a]
announceMaybeT f = do
  t <- elevate get
  (t', Endo e) <- runWriterT $ witherM (\obj -> mapMaybeT (lift . elevate) (runObject obj f)
      >>= \(x, obj') -> lift (writer (obj', Endo (x:)))) t
  elevate (put t')
  return (e [])

-- | An object which is specialized to be a Mealy machine
newtype Process m a b = Process { unProcess :: Object (Request a b) m }

-- | @_Process :: Iso' (Object (Request a b) m) (Process m a b)@
_Process :: (Profunctor p, Functor f) => p (Process m a b) (f (Process m a b)) -> (p (Object (Request a b) m) (f (Object (Request a b) m)))
_Process = dimap Process (fmap unProcess)

instance Functor f => Functor (Process f a) where
  fmap f (Process o0) = Process $ go o0 where
    go o = Object $ \(Request a cont) -> fmap (cont *** go) $ runObject o (Request a f)

instance Applicative f => Applicative (Process f a) where
  pure a = Process go where
    go = Object $ \(Request _ cont) -> pure (cont a, go)
  Process f0 <*> Process a0 = Process $ go f0 a0 where
    go mf ma = Object $ \(Request a cont) -> (\(f, mf') (x, ma') -> (cont (f x), go mf' ma'))
      <$> runObject mf (Request a id)
      <*> runObject ma (Request a id)

instance (Applicative f, Monoid b) => Monoid (Process f a b) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Monad m => C.Category (Process m) where
  id = arr id
  Process g0 . Process f0 = Process $ go f0 g0 where
    go f g = Object $ \(Request a cont) -> runObject f (Request a id)
      >>= \(b, f') -> liftM (\(c, g') -> (cont c, go f' g')) $ runObject g (Request b id)

instance Monad m => Arrow (Process m) where
  arr f = Process go where
    go = Object $ \(Request a cont) -> return (cont (f a), go)
  first (Process f0) = Process $ go f0 where
    go f = Object $ \(Request (a, c) cont) -> liftM (\(b, f') -> (cont (b, c), go f')) $ runObject f (Request a id)
  second (Process f0) = Process $ go f0 where
    go f = Object $ \(Request (a, c) cont) -> liftM (\(d, f') -> (cont (a, d), go f')) $ runObject f (Request c id)

instance Monad m => ArrowChoice (Process m) where
  left (Process f0) = Process $ go f0 where
    go f = Object $ \(Request e cont) -> case e of
      Left a -> liftM (\(b, f') -> (cont (Left b), go f')) $ runObject f (Request a id)
      Right c -> return (cont (Right c), go f)
  right (Process f0) = Process $ go f0 where
    go f = Object $ \(Request e cont) -> case e of
      Right a -> liftM (\(b, f') -> (cont (Right b), go f')) $ runObject f (Request a id)
      Left c -> return (cont (Left c), go f)

instance Monad m => Profunctor (Process m) where
  dimap f g (Process f0) = Process (go f0) where
    go m = Object $ \(Request a cont) -> liftM (\(b, m') -> (cont (g b), go m')) $ runObject m (Request (f a) id)
  {-# INLINE dimap #-}

instance Monad m => Strong (Process m) where
  first' = first
  {-# INLINE first' #-}
  second' = second
  {-# INLINE second' #-}

instance Monad m => Choice (Process m) where
  left' = left
  {-# INLINE left' #-}
  right' = right
  {-# INLINE right' #-}

instance (Applicative m, Num o) => Num (Process m i o) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance (Applicative m, Fractional o) => Fractional (Process m i o) where
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  recip = fmap recip
  fromRational = pure . fromRational
