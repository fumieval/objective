0.6.2
----
* Added `announce`, `announceMaybe` and `announceMaybeT` which invoke a method for every objects in a container.
* Renamed `(.>>.)` to `(@>>@)`, `.|>.` to `(@|>@)` for consistency.
* Added `filterPush`.
* Added `iterObject`, `iterTObject`, `iterative`, `iterativeT` for free monads.
* Renamed `runSequential` to `(@!)`.
* Added combinators for `ReifiedProgramT`: `(@!!)` and `sequentialT`.
* Changed the semantics of `variable` to accept `StateT`.

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
