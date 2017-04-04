For readability, `Object` and `runObject` are abbreviated as `inO`, `outO`.

Associativity
-------------------------------

For every type of action `e`, `f`, `g`,
Functor `h`, and for every object `a :: Object e f`,
`b :: Object f g`,
`c :: Object g h`,

```haskell
a @>>@ (b @>> c) == (a @>>@ b) @>>@ c
```

Proof:

```haskell
outO (a @>>@ (b @>>@ c))
= { Definition of (@>>@) }
fmap joinO . fmap joinO . outO c . outO b . outO a) f
= { fmap fusion }
fmap (joinO . joinO) . outO c . outO b . outO a
= { Expanding (joinO . joinO) }
= fmap (\(((x, ef), fg), gh) -> (x, ef @>>@ (fg @>>@ gh)))
        . outO c . outO b . outO a
```

```haskell
outO ((a @>>@ b) @>>@ c)
= { Definition of (@>>@) }
fmap joinO . outO c . (fmap joinO . outO b . outO a) f
= { outO . inO = id }
fmap joinO . outO c . fmap joinO . outO b . outO a
= { Naturality }
fmap joinO . fmap (first joinO) . outO c . outO b . outO a
= { fmap fusion }
fmap (joinO . first joinO) . outO c . outO b . outO a
= { Expansion }
= fmap (\(((x, ef), fg), gh) -> (x, (ef @>>@ fg) @>>@ gh))
        . outO c . outO b . outO a
= { Coinduction }
= fmap (\(((x, ef), fg), gh) -> (x, ef @>>@ (fg @>>@ gh)))
        . outO c . outO b . outO a
= { LHS }
outO (a @>>@ (b @>>@ c))
```

Left identity
-------------------------------

```haskell
outO (echo @>>@ obj)
= { Definition of echo }
fmap (\x -> (x, echo))) @>>@ obj
= { Definition of (@>>@) }
fmap joinO . outO obj . fmap (\x -> (x, echo))
= { Naturality }
fmap joinO . fmap (first (\x -> (x, echo))) . outO obj
= { fmap fusion }
fmap (joinO . first (\x -> (x, echo))) . outO obj
= { Definition of joinO }
fmap (\(x, m) -> (x, echo @>>@ m)) . outO obj
= { Coinduction }
fmap (\(x, m) -> (x, m)) . outO obj
= { fmap id = id }
outO obj
```

Right identity
-------------------------------
For every object `obj :: Object f g`, `obj @>>@ echo = obj`

```haskell
outO (obj @>>@ echo)
= { Definition of echo }
outO (obj @>>@ Object (fmap (\x -> (x, echo))))
= { Definition of (@>>@) }
fmap joinO . fmap (\x -> (x, echo)) . outO obj
= { Naturality }
fmap (joinO . (\x -> (x, echo))) . outO obj
= { fmap fusion }
fmap (\(x, m) -> (x, m @>>@ echo)) . outO obj
= { Coinduction }
fmap (\(x, m) -> (x, m)) . outO obj
= { fmap id = id }
outO obj
```
