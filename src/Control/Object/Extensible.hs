{-# LANGUAGE GADTs, DataKinds, TypeOperators, ScopedTypeVariables #-}
module Control.Object.Extensible where

import Control.Monad.Skeleton
import Data.Extensible.Internal
import Control.Object.Object

data Action xs a where
    Action :: !(Membership xs (s ':> t)) -> t a -> Action xs a

type Eff xs = Skeleton (Action xs)

liftEff :: forall proxy s t xs a. Associate s t xs => proxy s -> t a -> Eff xs a
liftEff _ = bone . Action (association :: Membership xs (s ':> t))

emptyObject :: Object (Eff '[]) m
emptyObject = Object $ const $ error "Impossbile"

(@<|@) :: Monad m => Object t m -> Object (Eff xs) m -> Object (Eff ((s ':> t) ': xs)) m
obj @<|@ base = Object $ \m -> case unbone m of
  Return a -> return (a, obj @<|@ base)
  Action p t :>>= k -> runMembership p
    (\Refl -> runObject obj t
      >>= \(a, obj') -> runObject (obj' @<|@ base) (k a))
    (\p' -> runObject base (bone $ Action p' t)
      >>= \(a, base') -> runObject (obj @<|@ base') (k a))

solo :: Monad m => Object t m -> Object (Eff '[s ':> t]) m
solo obj = Object $ \m -> case unbone m of
  Return a -> return (a, solo obj)
  Action p t :>>= k -> runMembership p
    (\Refl -> runObject obj t >>= \(a, obj') -> runObject (solo obj') (k a))
    (const $ error "Impossbile")
