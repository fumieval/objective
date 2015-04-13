{-# LANGUAGE CPP, Rank2Types, GADTs, ViewPatterns, LambdaCase, TemplateHaskell, TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds, KindSignatures, DataKinds #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Object.Extensible
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Extensible effects and objects
-----------------------------------------------------------------------------
module Control.Object.Extensible (Action(..)
  , Eff
  , liftEff
  , hoistEff
  , emptyObject
  , (@<|@)
  , solo
  , mkEffects
  , Assoc(..)
  , Associate
  , module Data.Proxy
  ) where

import Control.Monad.Skeleton
import Data.Extensible.Internal (Assoc(..), Associate(..), Membership(..), runMembership, (:~:)(..), compareMembership)
import Data.Proxy
import Control.Object.Object
import Language.Haskell.TH
import Data.Char
import Control.Monad
#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (foldMap)
#endif
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer

-- | The empty object does not accept any messages.
emptyObject :: Object (Eff '[]) m
emptyObject = Object $ const $ error "Impossbile"

-- | Extend an object
(@<|@) :: Monad m => Object t m -> Object (Eff xs) m -> Object (Eff ((s ':> t) ': xs)) m
obj @<|@ base = Object $ \m -> case unbone m of
  Return a -> return (a, obj @<|@ base)
  Action p t :>>= k -> runMembership p
    (\Refl -> runObject obj t
      >>= \(a, obj') -> runObject (obj' @<|@ base) (k a))
    (\p' -> runObject base (bone $ Action p' t)
      >>= \(a, base') -> runObject (obj @<|@ base') (k a))

-- | Promote an object so that it handles 'Eff'
--
-- @solo a = a '@<|@' 'emptyObject'@
solo :: Monad m => Object t m -> Object (Eff '[s ':> t]) m
solo obj = Object $ \m -> case unbone m of
  Return a -> return (a, solo obj)
  Action p t :>>= k -> runMembership p
    (\Refl -> runObject obj t >>= \(a, obj') -> runObject (solo obj') (k a))
    (const $ error "Impossbile")

-- | A unit of effects
data Action (xs :: [Assoc k (* -> *)]) a where
  Action :: !(Membership xs (s ':> t)) -> t a -> Action xs a

-- | The extensible operational monad
type Eff xs = Skeleton (Action xs)

-- | Lift some effect to 'Eff'
liftEff :: forall proxy s t xs a. Associate s t xs => proxy s -> t a -> Eff xs a
liftEff _ x = bone (Action (association :: Membership xs (s ':> t)) x)
{-# INLINE liftEff #-}

hoistEff :: forall proxy s t xs a. Associate s t xs => proxy s -> (forall x. t x -> t x) -> Eff xs a -> Eff xs a
hoistEff _ f = hoistSkeleton $ \(Action i t) -> case compareMembership (association :: Membership xs (s ':> t)) i of
  Right Refl -> Action i (f t)
  _ -> Action i t
{-# INLINABLE hoistEff #-}

instance Associate "Reader" (Reader r) xs => MonadReader r (Eff xs) where
  ask = liftEff (Proxy :: Proxy "Reader") ask
  {-# INLINE ask #-}
  local f = hoistEff (Proxy :: Proxy "Reader") (local f)
  {-# INLINE local #-}

instance Associate "State" (State s) xs => MonadState s (Eff xs) where
  get = liftEff (Proxy :: Proxy "State") get
  {-# INLINE get #-}
  put s = liftEff (Proxy :: Proxy "State") (put s)
  {-# INLINE put #-}
  state f = liftEff (Proxy :: Proxy "State") (state f)
  {-# INLINE state #-}

instance (Monoid w, Associate "Writer" (Writer w) xs) => MonadWriter w (Eff xs) where
  writer a = liftEff (Proxy :: Proxy "Writer") (writer a)
  {-# INLINE writer #-}
  tell w = liftEff (Proxy :: Proxy "Writer") (tell w)
  {-# INLINE tell #-}
  listen = go mempty where
    go w m = case unbone m of
      Return a -> return (a, w)
      Action i t :>>= k -> case compareMembership (association :: Membership xs ("Writer" ':> Writer w)) i of
        Right Refl -> bone (Action i t) >>= go (w <> execWriter t) . k
        Left _ -> bone (Action i t) >>= go w . k
  pass m = listen m >>= \((a, f), w) -> writer (a, f w)
  {-# INLINABLE pass #-}

-- | Generate named effects from a GADT declaration.
mkEffects :: Name -> DecsQ
mkEffects name = reify name >>= \case
  TyConI (DataD _ _ (fmap getTV -> tyvars) cs _)
    | not (null tyvars) -> fmap concat $ forM cs $ \case
      NormalC con st -> mk tyvars [] con st
      ForallC _ eqs (NormalC con st) -> mk tyvars eqs con st
      p -> do
        runIO (print p)
        fail "Unsupported constructor"
  _ -> fail "mkEffects accepts GADT declaration"
  where
    mk tyvars eqs con (fmap snd -> argTypes) = do
#if MIN_VERSION_template_haskell(2,10,0)
      let dic_ = [(v, t) | AppT (AppT EqualityT (VarT v)) t <- eqs]
#else
      let dic_ = [(v, t) | EqualP (VarT v) t <- eqs]
#endif
      let dic = dic_ ++ [(t, VarT v) | (v, VarT t) <- dic_]

      let tvs = map mkName $ concatMap (flip replicateM ['a'..'z']) [1..]

      let params' = do
            (t, v) <- zip tyvars tvs
            case lookup t dic of
              Just (VarT p) -> return (t, p)
              _ -> return (t, v)

      let (bts, fts) = foldMap (\(p, t) -> maybe ([VarT t], [t]) (\case
              VarT _ -> ([VarT t], [t])
              x -> ([x], [])) (lookup p dic)) (init params')

      let argTypes' = map (\case
            VarT n -> maybe (VarT n) VarT $ lookup n params'
            x -> x) argTypes

      let (extra, result) = case lookup (last tyvars) dic of
            Just (VarT v) -> (id, case lookup v params' of
              Just p -> VarT p
              Nothing -> VarT v)
            Just t -> (id, t)
            Nothing -> ((PlainTV (mkName "x"):), VarT $ mkName "x")

      -- Eff xs R
      let rt = ConT ''Eff `AppT` VarT (mkName "xs") `AppT` result

      -- a -> B -> C -> Eff xs R
      let fun = foldr (\x y -> ArrowT `AppT` x `AppT` y) rt argTypes'

      -- Foo a B
      let eff = foldl AppT (ConT name) bts

      -- "Foo"
      let nameT = LitT $ StrTyLit $ nameBase name

#if MIN_VERSION_template_haskell(2,10,0)
      -- Associate "Foo" (Foo a B) xs
      let cx = ConT ''Associate
            `AppT` nameT
            `AppT` eff
            `AppT` VarT (mkName "xs")
#else
      let cx = ClassP ''Associate [nameT, eff, VarT (mkName "xs")]
#endif

      let typ = ForallT (PlainTV (mkName "xs") : extra (map PlainTV fts)) [cx] fun

      -- liftEff (Proxy :: Proxy "Foo")
      let lifter = VarE 'liftEff `AppE` (ConE 'Proxy `SigE` AppT (ConT ''Proxy) nameT)

      let argNames = map (mkName . ("a" ++) . show) [0..length argTypes-1]

      let ex = lifter `AppE` foldl AppE (ConE con) (map VarE argNames)

      let fName = let (ch : rest) = nameBase con in mkName $ toLower ch : rest
      return [SigD fName typ
        , FunD fName [Clause (map VarP argNames) (NormalB ex) []]]

    getTV (PlainTV n) = n
    getTV (KindedTV n _) = n
