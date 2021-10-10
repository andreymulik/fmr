# FMR library changelog

## Version 0.2

* New acronym: **Fields for Monadic Records**
* New `Prop` is [existential](https://wiki.haskell.org/Existential_type), so it
can be extended by new field types using `Data.Property.IsProp` class

* `Prop` constructors `:=`, `:~`, `::=` and `::~` replaced by patterns
* `Prop` constructors `Incr` and `Decr` replaced by `incr` and `decr` fields

* `InsertProp` and `DeleteProp` classes replaced by `SDP.LinearM.LinearM` class
(since `sdp-0.2.1`)
* `Prop` constructors `:+=`, `:=+` and `:~=` moved to `SDP.LinearM` module of
library `sdp` (since `sdp-0.2.1`)

* Added new classes `IsMVar` and `MonadVar`
* Added new field `this` in module `Data.Field`
* Added new fields `switch`, `incr` and `decr` in module `Data.Property`

* Removed `SField` type.
* Removed `setProp` function in module `Data.Property`
* Removed `incRecord` and `decRecord` methods in `SwitchProp` class
* Removed `OSField` type synonym and `sfield` function (use `SField` instead)

## Version 0.1

* Added `Observe`, `SField`, `Field` and `Prop` types
* Added `SwitchProp`, `GetProp`, `SetProp`, `ModifyProp`, `InsertProp` and
`DeleteProp` classes
