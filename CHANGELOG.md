# 1.4 (2015-09-19)

- Infix operators
- `meets` and `joins` generalised to work on any `Foldable`
- Deprecate `Algebra.Enumerable`, use [universe package](http://hackage.haskell.org/package/universe)
- Add `Applicative` and `Monad` typeclasses to `Dropped`, `Lifted` and `Levitated`
- Add `Semigroup` instance to `Join` and `Meet`
- Add instances for `()`, `Proxy`, `Tagged` and `Void`

# 1.3 (2015-05-18)

- relaxed constraint for `BoundedLattice (Levitated a)`
- added instances to `Dropped`, `Levitated` and `Lifted`:
    - from base
    - `NFData`
    - `Hashable`
- added `HashSet` and `HashMap` lattice instances
