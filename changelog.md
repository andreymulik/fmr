# FMR library changelog

## Version 0.2

* Now extensible! The new definition of `Prop` allows you to extend the
library's capabilities by creating new types of fields.

* New acronym: **"Fields for Monadic Records"** instead of
**"Fake Monadic Records"**.

* The `InsertProp` and `DeleteProp` classes have been replaced by `FieldLinear`,
defined in `SDP.LinearM` module of library `sdp` (since `sdp-0.2.1`).

* Added new class `Data.Property.IsProp` with method `performProp`.
* Added new fields `switch`, `incr` and `decr` in module `Data.Property`.
* Added new class `Data.Field.ThisProp` with field `this` for mutable variables
and structures. Now `ThisProp` have instances for `STRef`, `IORef`, `MVar` and
`TVar`, also see `sdp-0.2.1`.

* Type `SField` replaced by `Field` pattern synonym
* Removed `setProp` function in module `Data.Property`
* Removed `OSField` type synonym and `sfield` function
* Removed `incRecord` and `decRecord` methods in `SwitchProp` class

* `Prop` constructors removed (`Incr`, `Decr`, `Switch`, `:=+`, `:+=` and `:~=`)
or replaced by **write-only** pattern synonyms (`:=`, `::=`, `:~` and `::~`).
* Added `Prop` [existential](https://wiki.haskell.org/Existential_type)
constructor for type `Prop`.

## Version 0.1

* Added `SwitchProp`, `GetProp`, `SetProp`, `ModifyProp`, `InsertProp` and
`DeleteProp` classes
* Added `Observe`, `SField`, `Field` and `Prop` types

