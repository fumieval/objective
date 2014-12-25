{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE Rank2Types, CPP, TypeOperators #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DeriveDataTypeable #-}
#endif
module Control.Object.Object where
import Data.Functor.Day
import Data.Functor.Coproduct
import Control.Monad
import Control.Monad.Free
import Control.Monad.Operational.Mini
import qualified Control.Monad.Trans.Free as T
import qualified Control.Monad.Trans.Operational.Mini as T
import Control.Monad.Trans.State.Strict
import Data.OpenUnion1.Clean
import Data.Typeable
import Control.Applicative

-- | The type @Object f g@ represents objects which can handle messages @f@, perform actions in the environment @g@.
-- It can be thought of as an automaton that converts effects.
-- 'Object's can be composed just like functions using '@>>@'; the identity element is 'echo'.
-- Objects are morphisms of the category of functors
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
objectTyCon = mkTyCon3 "objective" "Control.Object" "Object"
#endif
{-# NOINLINE objectTyCon #-}
#endif

-- | An alias for 'runObject'.
(@-) :: Object f g -> f x -> g (x, Object f g)
(@-) = runObject
{-# INLINE (@-) #-}
infixr 3 @-

-- | The identity object
echo :: Functor f => Object f f
echo = Object (fmap (\x -> (x, echo)))

-- | Lift a natural transformation into an object.
liftO :: Functor g => (forall x. f x -> g x) -> Object f g
liftO f = go where go = Object $ fmap (\x -> (x, go)) . f
{-# INLINE liftO #-}

-- | The unwrapped analog of 'stateful'
--     @unfoldO runObject = id@
--     @unfoldO runSequential = sequential@
--     @unfoldO iterObject = iterable@
unfoldO :: Functor g => (forall a. r -> f a -> g (a, r)) -> r -> Object f g
unfoldO h = go where go r = Object $ fmap (fmap go) . h r
{-# INLINE unfoldO #-}

unfoldOM :: Monad m => (forall a. r -> f a -> m (a, r)) -> r -> Object f m
unfoldOM h = go where go r = Object $ liftM (fmap go) . h r
{-# INLINE unfoldOM #-}

-- | Build a stateful object.
--
-- @stateful t s = t ^>>@ variable s@
stateful :: Monad m => (forall a. f a -> StateT s m a) -> s -> Object f m
stateful h = go where
  go s = Object $ liftM (\(a, s') -> (a, go s')) . flip runStateT s . h
{-# INLINE stateful #-}

-- | Object-object composition
(@>>@) :: Functor h => Object f g -> Object g h -> Object f h
Object m @>>@ Object n = Object $ fmap (\((x, m'), n') -> (x, m' @>>@ n')) . n . m
infixr 1 @>>@

-- | Reversed '(@>>@)'
(@<<@) :: Functor h => Object g h -> Object f g -> Object f h
(@<<@) = flip (@>>@)
{-# INLINE (@<<@) #-}
infixl 1 @<<@


-- | Object-function composition
(@>>^) :: Functor h => Object f g -> (forall x. g x -> h x) -> Object f h
m0 @>>^ g = go m0 where go (Object m) = Object $ fmap (fmap go) . g . m
infixr 1 @>>^

-- | Function-object composition
(^>>@) :: Functor h => (forall x. f x -> g x) -> Object g h -> Object f h
f ^>>@ m0 = go m0 where go (Object m) = Object $ fmap (fmap go) . m . f
infixr 1 ^>>@

-- | Parallel composition
(@**@) :: Applicative m => Object f m -> Object g m -> Object (Day f g) m
a @**@ b = Object $ \(Day f g r) -> (\(x, a') (y, b') -> (r x y, a' @**@ b')) <$> runObject a f <*> runObject b g
infixr 3 @**@

-- | Objective fanin
(@||@) :: Functor m => Object f m -> Object g m -> Object (Coproduct f g) m
a @||@ b = Object $ \(Coproduct r) -> case r of
  Left f -> fmap (fmap (@||@b)) (runObject a f)
  Right g -> fmap (fmap (a@||@)) (runObject b g)
infixr 2 @||@

-- | An object that won't accept any messages.
loner :: Functor f => Object Nil f
loner = liftO exhaust

-- | Extend an object by another independent object.
(@|>@) :: Functor g => Object f g -> Object (Union s) g -> Object (f |> Union s) g
p @|>@ q = Object $ fmap (fmap (@|>@q)) . runObject p ||> fmap (fmap (p @|>@)) . runObject q
infixr 3 @|>@

-- | Build a stateful object, sharing out the state.
sharing :: Monad m => (forall a. f a -> StateT s m a) -> s -> Object (State s |> f |> Nil) m
sharing m = go where
  go s = Object $ \k -> liftM (fmap go) $ ($k)
    $ (\n -> return $ runState n s)
    ||> (\e -> runStateT (m e) s)
    ||> exhaust
{-# INLINE sharing #-}

(@!) :: Monad m => Object e m -> ReifiedProgram e a -> m (a, Object e m)
obj @! Return a = return (a, obj)
obj @! (e :>>= cont) = runObject obj e >>= \(a, obj') -> obj' @! cont a
infixr 3 @!

(@!!) :: Monad m => Object e m -> T.ReifiedProgramT e m a -> m (a, Object e m)
obj @!! T.Return a = return (a, obj)
obj @!! T.Lift m cont = m >>= (obj @!!) . cont
obj @!! (e T.:>>= cont) = runObject obj e >>= \(a, obj') -> obj' @!! cont a
infixr 3 @!!

iterObject :: Monad m => Object f m -> Free f a -> m (a, Object f m)
iterObject obj (Pure a) = return (a, obj)
iterObject obj (Free f) = runObject obj f >>= \(cont, obj') -> iterObject obj' cont

iterTObject :: Monad m => Object f m -> T.FreeT f m a -> m (a, Object f m)
iterTObject obj m = T.runFreeT m >>= \r -> case r of
  T.Pure a -> return (a, obj)
  T.Free f -> runObject obj f >>= \(cont, obj') -> iterTObject obj' cont

-- | Let object handle 'ReifiedProgram'.
sequential :: Monad m => Object e m -> Object (ReifiedProgram e) m
sequential = unfoldOM (@!)

-- | Let object handle 'ReifiedProgramT'.
sequentialT :: Monad m => Object e m -> Object (T.ReifiedProgramT e m) m
sequentialT = unfoldOM (@!!)

iterative :: Monad m => Object f m -> Object (Free f) m
iterative = unfoldOM iterObject

iterativeT :: Monad m => Object f m -> Object (T.FreeT f m) m
iterativeT = unfoldOM iterTObject

-- | Change the workspace of the object.
transObject :: Functor g => (forall x. f x -> g x) -> Object e f -> Object e g
transObject f = (@>>^f)

-- | Apply a function to methods coming into an object.
adaptObject :: Functor m => (forall x. g x -> f x) -> Object f m -> Object g m
adaptObject f = (f^>>@)
