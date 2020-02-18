# 2.0.2 (2020-02-18)

- Add `Algebra.Lattice.Stacked`
  [#99](https://github.com/phadej/lattices/pull/99)

# 2.0.1 (2019-07-22)

- Add `(PartialOrd a, PartialOrd b) => PartialOrd (Either a b)` instance

# 2 (2019-04-17)

- Reduce to three classes (from six): `Lattice`, `BoundedMeetSemiLattice`,
  `BoundedJoinSemiLattice`.
  The latter two names are kept to help migration.
- Remove `Algebra.Enumerable` module. Use `universe` package.
- Drop GHC-7.4.3 support (broken `ConstraintKinds`)
- Move `Algebra.Lattice.Free` to `Algebra.Lattice.Free.Final`
- Add concrete syntax `Algebra.Lattice.Free` and `Algebra.Heyting.Free` using
  LJT-proof search for `Eq` and `PartialOrd`
- Change `PartialOrd [a]` to be `leq = isSubsequenceOf`

# 1.7.1.1 (2019-07-05)

- Allow newer dependencies, update cabal file

# 1.7.1 (2018-01-29)

- Correct *Safe Haskell* annotations. See https://github.com/ekmett/semigroupoids/issues/69
- Bump lower bounds

# 1.7 (2017-10-01)

- `HashMap` instances changed
- `PartialOrd Meet` and `Join`
- `PartialOrd ()` and `Void`
- `BoundedLattice (HashSet a)`
- `PartialOrd [a]` (`leq = isInfixOf`)

# 1.6.0 (2017-06-26)

- Correct PartialOrd Map and IntMap instances
- Add Lattice instance for `containers` types.
- Change `meets1` and `joins1` to use `Foldable1`
- Add `comparable` to `PartialOrd`
- Add `Algebra.Lattice.Free` module
- Add `Divisibility` lattice.
- Fix `Lexicographic`.

# 1.5.0 (2015-12-18)

- Move `PartialOrd (k -> v)` instance into own module
- `Const` and `Identity` instances
- added `fromBool`
- Add `Lexicographic`, `Ordered` and `Op` newtypes

# 1.4.1 (2015-10-26)

- `MINIMAL` pragma in with GHC 7.8
- Add `DEPREACTED` pragma for `meet` and `join`,
  use infix version `\/` and `/\`

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
