1.3
----
* Supported GHC 9.0
* Removed `unfoldOM`
* Removed `apprisesOf`
* Removed `withBuilder`
* Trimmed unnecessary dependencies

1.2
----

* Removed `iterObject` and `iterative`
* Removed `Data.Functor.Request`

1.1.2
----

* Removed `either` dependency

1.1
----
* Removed `HProfunctor`, `(..-)`, `invokeOnSTM`, `newSTM`, `snapshot`
* Added `(?-)`

1.0.5
----
* Added `filterO` and `filteredO`
* Renamed `announcesOf` to `invokesOf`
* Added `invokes`
* Added `(@!=)`
* Shaved unnecessary `Monoid r` off in `apprisesOf`

1.0.4
----
* Simplified `Instance`
* Added `snapshot`
* Added `cascade`, `cascadeObject`
* Added `announcesOf`
* Added `(@||@)`
* Safe Haskell

1.0.3
----
* Added `apprisesOf`
* Added `(@~)`

1.0.2
----
* Made `(.-)` exception-safe

1.0.1
----
* Switched to use TMVar for instances
* Added atomic operations
* Add `apprises`
* Re-added `accept`
* Added `(>~~>)`
* Added `accumulator`

1.0
----
* No longer support `extensible`, `elevator`, and `minioperational`
* Removed `Data.Functor.PushPull`
* Removed `Control.Object.Process`
* Removed `Control.Object.Stream`
* Removed `Control.Monad.Objective`
* Added `apprise`
* Generalized `(^>>@)` and `(@>>^)` so that they also work on instances

0.6.5
----
* Supported `elevator >= 0.2`.

0.6.3.1
----
* Reverted the fixity.

0.6.3
----
* Added `Variable`, an alias for `variable` objects.
* Added `Mortal`.
* Increased fixity of invocation operators to 5.
* Added stream connection operators.
* Added `transit` and `animate`.

0.6.2
----
* Added `announce`, `announceMaybe` and `announceMaybeT` which invoke a method for every objects in a container.
* Added `(@**@)` and `(@||@)`.
* Renamed `(.>>.)` to `(@>>@)`, `.|>.` to `(@|>@)` for consistency.
* Added `filterPush`, `bipush`, `bipull`.
* Added `iterObject`, `iterTObject`, `iterative`, `iterativeT` for free monads.
* Renamed `runSequential` to `(@!)`.
* Added combinators for `ReifiedProgramT`: `(@!!)` and `sequentialT`.
* Changed the semantics of `variable` to accept `StateT`.
* Added `flyweight'` that relies on HashMap.
* Added `MonadObjective` constraint

0.6.1
-----
* Fixed the wrong constraints of `request`

0.6
-----
* `PushPull` has more `Floors`
* Added `Applicative` instance for `Request`
* Reformed around Control.Monad.Objective
  * Instance f g m ==> Inst m f g
  * `invoke` takes two transformations for lifting
  * Added lifted versions of `new`: `newIO` and `newST`

0.5.2
-----
* Added Process
* Added `runSequential`, `sequential` for operational monad
* Added `flyweight`

0.5.1
-----
* Added PushPull functor
* Removed `sequential`

0.5
-----
* Lift has gone
* Use elevator instead of Lift
* Moved Request to a separate module

0.4
-----
* Added Request functor along with Lift
* Supported "extensible" objects using open unions
* AccessT is now obsolete
