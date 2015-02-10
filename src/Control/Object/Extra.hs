{-# LANGUAGE Rank2Types, TypeOperators, FlexibleContexts, ConstraintKinds #-}
module Control.Object.Extra where
import Control.Object.Object
import qualified Data.HashMap.Strict as HM
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Monad
import Data.Functor.Request
import Control.Applicative
import Data.Monoid
import Data.Hashable
import Data.Traversable as T

-- | Like 'flyweight', but it uses 'Data.HashMap.Strict' internally.
flyweight :: (Applicative m, Eq k, Hashable k) => (k -> m a) -> Object (Request k a) m
flyweight f = go HM.empty where
  go m = Object $ \(Request k cont) -> case HM.lookup k m of
    Just a -> pure (cont a, go m)
    Nothing -> (\a -> (cont a, go $ HM.insert k a m)) <$> f k
{-# INLINE flyweight #-}

animate :: (Applicative m, Num t) => (t -> m a) -> Object (Request t a) m
animate f = go 0 where
  go t = Object $ \(Request dt cont) -> (\x -> (cont x, go (t + dt))) <$> f t
{-# INLINE animate #-}

transit :: (Alternative m, Fractional t, Ord t) => t -> (t -> m a) -> Object (Request t a) m
transit len f = go 0 where
  go t
    | t >= len = Object $ const empty
    | otherwise = Object $ \(Request dt cont) -> (\x -> (cont x, go (t + dt))) <$> f (t / len)
{-# INLINE transit #-}

announce :: (T.Traversable t, Monad m) => f a -> StateT (t (Object f m)) m [a]
announce f = StateT $ \t -> do
  (t', Endo e) <- runWriterT $ T.mapM (\obj -> lift (runObject obj f)
      >>= \(x, obj') -> writer (obj', Endo (x:))) t
  return (e [], t')

-- | A mutable variable.
variable :: Monad m => s -> Object (StateT s m) m
variable s = Object $ \m -> liftM (fmap variable) $ runStateT m s
