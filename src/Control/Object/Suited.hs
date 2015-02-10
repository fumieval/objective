{-# LANGUAGE PolyKinds, Rank2Types, TypeOperators, ScopedTypeVariables, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Object.Suited
-- Copyright   :  (c) Fumiaki Kinoshita 2015
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Control.Object.Suited (Suited(..), guest) where
import Control.Object.Object
import Data.Functor.Coproduct
import Data.Extensible
import Control.Monad.Free
import Data.Proxy
import Unsafe.Coerce
import Control.Arrow

class Suited g f where
  suited :: Object f g

newtype Flipped f a b = Flipped { unFlipped :: f b a }

instance (Functor g, Forall (Suited g) fs) => Suited g (Union fs) where
  suited = magazine $ generateFor (Proxy :: Proxy (Suited g)) $ const (Flipped suited)

instance (Functor h, Suited h f, Suited h g) => Suited h (Coproduct f g) where
  suited = suited @||@ suited

instance (Monad g, Suited g f) => Suited g (Free f) where
  suited = iterative suited

magazine :: Functor g => Flipped Object g :* fs -> Object (Union fs) g
magazine table = Object $ \(Union (UnionAt pos (Flux f h))) -> fmap (fmap magazine) $ runPairT
    $ sectorAt pos (\obj -> PairT $ fmap (f *** Flipped) $ runObject (unFlipped obj) h) table

newtype Guest f g r = Guest (Suited g f => r)

guest :: forall f g r. Object f g -> (Suited g f => r) -> r
guest a k = unsafeCoerce (Guest k :: Guest f g r) a

newtype PairT m a b = PairT { runPairT :: m (a, b) } deriving Functor
