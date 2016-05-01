objective
====

[![Hackage](https://img.shields.io/hackage/v/objective.svg)](https://hackage.haskell.org/package/objective) [![Build Status](https://secure.travis-ci.org/fumieval/objective.png?branch=master)](http://travis-ci.org/fumieval/objectve)

Paper: https://fumieval.github.io/papers/en/2015-Haskell-objects.html

This package provides composable objects and instances.

Introduction
----

The primal construct, `Object`, models _object-oriented_ objects. `Object f g` represents an object.

```haskell
newtype Object f g = Object { runObject :: forall x. f x -> g (x, Object f g) }
```

An object interprets a message `f a` and returns the result `a` and the next object `Object f g`, on `g`.

```haskell
data Counter a where
  Increment :: Counter ()
  Print :: Counter ()

counter :: Int -> Object Counter IO
counter n = Object $ \case
  Increment -> return ((), counter (n + 1))
  Print -> print n >> return (n, counter n)
```

`new :: Object f g -> IO (Instance f g)` creates an instance of an object.

`(.-) :: (MonadIO m, MonadMask m) => Instance f m -> f a -> m a` sends a message to an instance. This can be used to handle instances in the typical OOP fashion.

```haskell
> i <- new (counter 0)
> i .- Increment
> i .- Print
1
> i .- Increment
> i .- Print
2
```

Interestingly, `Object (Skeleton t) m` and `Object t m` are isomorphic (`Skeleton` is an operational monad). `cascading` lets objects to handle an operational monad.
