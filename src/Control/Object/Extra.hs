{-# LANGUAGE Rank2Types, TypeOperators, FlexibleContexts, ConstraintKinds #-}
module Control.Object.Extra where
import Control.Object.Object
import Control.Object.Mortal
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Data.Witherable
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Monad
import Control.Elevator
import Data.Functor.Request
import Data.Functor.PushPull
import Control.Applicative
import Data.OpenUnion1.Clean
import Data.Monoid
import Data.Hashable
import Data.Traversable as T
import Data.IORef

-- | Build an object using continuation passing style.
oneshot :: (Functor f, Monad m) => (forall a. f (m a) -> m a) -> Object f m
oneshot m = go where
  go = Object $ \e -> m (fmap return e) >>= \a -> return (a, go)
{-# INLINE oneshot #-}

-- | The flyweight pattern.
flyweight :: (Monad m, Ord k) => (k -> m a) -> Object (Request k a) m
flyweight f = go Map.empty where
  go m = Object $ \(Request k cont) -> case Map.lookup k m of
    Just a -> return (cont a, go m)
    Nothing -> f k >>= \a -> return (cont a, go $ Map.insert k a m)

-- | Like 'flyweight', but it uses 'Data.HashMap.Strict' internally.
flyweight' :: (Monad m, Eq k, Hashable k) => (k -> m a) -> Object (Request k a) m
flyweight' f = go HM.empty where
  go m = Object $ \(Request k cont) -> case HM.lookup k m of
    Just a -> return (cont a, go m)
    Nothing -> f k >>= \a -> return (cont a, go $ HM.insert k a m)

animate :: (Applicative m, Num t) => (t -> m a) -> Object (Request t a) m
animate f = go 0 where
  go t = Object $ \(Request dt cont) -> (\x -> (cont x, go (t + dt))) <$> f t

transit :: (Alternative m, Fractional t, Ord t) => t -> (t -> m a) -> Object (Request t a) m
transit len f = go 0 where
  go t
    | t >= len = Object $ const empty
    | otherwise = Object $ \(Request dt cont) -> (\x -> (cont x, go (t + dt))) <$> f (t / len)

announce :: (T.Traversable t, Monad m, Elevate (State (t (Object f g))) m, Elevate g m) => f a -> m [a]
announce f = do
  t <- elevate get
  (t', Endo e) <- runWriterT $ T.mapM (\obj -> (lift . elevate) (runObject obj f)
      >>= \(x, obj') -> writer (obj', Endo (x:))) t
  elevate (put t')
  return (e [])

announceMaybe :: (Witherable t
    , Monad m
    , Elevate (State (t (Object f Maybe))) m) => f a -> m [a]
announceMaybe f = elevate $ state
  $ \t -> let (t', Endo e) = runWriter
                $ witherM (\obj -> case runObject obj f of
                  Just (x, obj') -> lift $ writer (obj', Endo (x:))
                  Nothing -> mzero) t in (e [], t')

announceMaybeT :: (Witherable t
  , Monad m
  , State (t (Object f (MaybeT g))) ∈ Floors1 m
  , g ∈ Floors1 m
  , Tower m) => f a -> m [a]
announceMaybeT f = do
  t <- elevate get
  (t', Endo e) <- runWriterT $ witherM (\obj -> mapMaybeT (lift . elevate) (runObject obj f)
      >>= \(x, obj') -> lift (writer (obj', Endo (x:)))) t
  elevate (put t')
  return (e [])

announceMortal :: (Witherable t
  , Monad m
  , State (t (Mortal f g ())) ∈ Floors1 m
  , g ∈ Floors1 m
  , Tower m) => f a -> m [a]
announceMortal f = do
  t <- elevate get
  (t', Endo e) <- runWriterT $ witherM (\obj -> MaybeT (lift $ liftM is $ elevate $ runMortal obj f)
      >>= \(x, obj') -> lift (writer (obj', Endo (x:)))) t
  elevate (put t')
  return (e [])
  where
    is (Left ()) = Nothing
    is (Right a) = Just a

type Variable s = forall m. Monad m => Object (StateT s m) m

-- | A mutable variable.
variable :: s -> Variable s
variable s = Object $ \m -> liftM (fmap variable) $ runStateT m s

moore :: Applicative f => (a -> r -> f r) -> r -> Object (PushPull a r) f
moore f = go where
  go r = Object $ \pp -> case pp of
    Push a c -> fmap (\z -> (c, z `seq` go z)) (f a r)
    Pull cont -> pure (cont r, go r)
{-# INLINE moore #-}

foldPP :: Applicative f => (a -> r -> r) -> r -> Object (PushPull a r) f
foldPP f = go where
  go r = Object $ \pp -> case pp of
    Push a c -> let z = f a r in pure (c, z `seq` go z)
    Pull cont -> pure (cont r, go r)
{-# INLINE foldPP #-}

(*-) :: IORef (Object f IO) -> f a -> IO a
r *- f = do
  obj <- readIORef r
  (a, obj') <- runObject obj f
  writeIORef r obj'
  return a
