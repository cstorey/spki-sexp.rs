SPKI S-Expressions
==================

[![Build Status](https://travis-ci.org/cstorey/spki-sexp.rs.svg?branch=master)](https://travis-ci.org/cstorey/spki-sexp.rs)

Intent
------
Intended to be a s-expression encoding (using [Ron Rivest's
grammar](http://people.csail.mit.edu/rivest/Sexp.txt)) for serialising Rust
types using Serde. Currently absurdly incomplete and likely to panic at the
slightest provocation.

Somewhat inspired by both LShift's
[Bletchley](https://github.com/lshift/bletchley) and the experimental
[hop](https://github.com/tonyg/hop) broker, this project is intended for use
as a wire-format, which is easy to implement in different languages.


Caveats
-------
The information model for Rivest's s-expressions, has only three variants:

```rust
pub enum SexpInfo {
  Atom(Vec<u8>),
  HintedAtom(Vec<u8>, Vec<u8>),
  List(Vec<&SexpInfo>)
}
```
Serde seems to (mostly) assume that the serialized representation is tagged
with the static type of the information being stored. So, for example, we
need to ensure that we can differentiate between a say plain string and a
number. So we represent an unsigned integer as `(uint 42)` instead of `42` for
this very reason. It'd make sense however to use the hinted bytes, and use
something like `[uint]42`. But as with many things, this is very much a work in progress. So, the usual warning applies.

Here be dragons, beware of dog.
