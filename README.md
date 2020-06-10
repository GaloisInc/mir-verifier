[![Build Status](https://travis-ci.org/GaloisInc/mir-verifier.svg?branch=master)](https://travis-ci.org/GaloisInc/mir-verifier)

# crux-mir

This is a static simulator for Rust programs.  It runs a set of test cases and
attempts to prove that all assertions pass on all valid inputs.


## Preliminaries

You must have the most recent version of the `mir-json` executable in your
path.  See [the `mir-json` README][mir-json-readme] for installation
instructions.

`mir-verifier` uses several submodules, so make sure they're initialized:

    $ git submodule update --init

[mir-json-readme]: https://github.com/GaloisInc/mir-json#readme


## Installation

Use ghc-8.4.4 or ghc-8.6.5.

    $ cabal v2-build

Then translate the Rust libraries in `lib/`:

    $ ./translate_libs.sh


## Usage

### Writing test cases

`crux-mir` looks for functions with the `#[crux_test]` attribute and runs them
as tests.  You may need to add `#![feature(custom_attribute)]` to the crate
root to use the `#[crux_test]` attribute.  These can both be conditionally
compiled by checking for `#[cfg(crux)]`.

Test cases can create and manipulate symbolic values using the functions in the
[`crucible`](lib/crucible) Rust crate.  See
[`example/ffs/lib.rs`](example/ffs/lib.rs) or the files in
[`test/symb_eval/`](test/symb_eval) for examples of creating symbolic values
and asserting properties about them.

### Running on a Cargo project

First, install the `crux-mir` binary to your `~/.cabal/bin` directory:

    $ cabal v2-install crux-mir

Set the `CRUX_RUST_LIBRARY_PATH` environment variable to the path to the
translated libraries:

    $ export CRUX_RUST_LIBRARY_PATH=.../mir-verifier/rlibs

In the directory of a Cargo project, run the project's symbolic tests:

    $ cargo crux-test

`cargo-crux-test` (part of `mir-json`) will translate the code to MIR, then
invoke `crux-mir` to symbolically simulate the test cases.

### Running on a single file

To compile and test a single Rust program:

    $ cabal v2-exec -- crux-mir test/conc_eval/prim/add1.rs

(Should print 2.)


## Test suite

To run the tests:

    $ cabal v2-test

### Expected Failures

Some tests are not yet expected to succeed, as crux-mir is still under
development. These tests are marked accordingly, so that the entire
test suite is still expected to pass.

Files that are not yet expected to work correctly begin with `// FAIL: ` and
a brief comment describing the reason for the expected failure.


## Language and Library Support

`crux-mir` currently supports most safe Rust language features and supports a
significant subset of the Rust standard library.

Currently supported:

 * Basic functionality: structs, enums, functions, pattern matching, and other
   control flow.
 * Common collections: slices, `Vec`, `str`, and `String` all generally work.
   However, some less-common methods on these types may not be supported yet.
 * Generics, traits, associated types, and specialization.
 * Closures.  However, certain conversions are unsupported: `crux-mir` cannot
   yet handle converting a closure to a function pointer or converting a
   `FnOnce` closure to `Fn` or `FnMut`.
 * Iterators and iterator adapters.  (This is just a side effect of support for
   generics and closures.)
 * Heap allocation with `Box`.

Partially supported:

 * Trait objects.  `&dyn Trait` and `&mut dyn Trait` types mostly work;
   `Box<dyn Trait>` and `Rc<dyn Trait>` are not yet supported.
 * Raw pointers.  Converting a reference to a raw pointer and back is
   supported.  Pointer arithmetic is supported only within arrays.  Pointer
   comparison with `==` is supported (useful for writing iterators).
   Integer-to-pointer casts are supported, but pointer-to-integer and
   pointer-to-pointer casts (except unsizing casts) are not.
 * Dynamically-sized types (DSTs).  Array-to-slice unsizing is supported,
   including via `CoerceUnsized` (so converting `Box<[T; 3]>` to `Box<[T]>`
   works).  Unsizing to a trait object works only for `&` and `&mut`.  Custom
   DST structs (that is, structs that have a dynamically-sized type as the last
   field) are not supported.

Not supported:

 * Running destructors (`Drop`).  Currently all variables are leaked (as with
   `mem::forget`) when going out of scope.
 * Multithreading.  `Arc`, atomics, `Mutex`, and `RwLock` are all unsupported.
 * `transmute` and equivalents.  In general, `crux-mir` does not yet support
   reinterpreting the bytes of a value of type `T` as a value of type `U`.
   This means `mem::transmute`, casts from `*const T` to `*const U`, and
   unions are all unsupported.
 * Calls to `extern` functions.
 * I/O functions.


## Built-in Checks

`crux-mir` automatically performs the following checks on each test case, even
in the absence of an explicit `crucible_assert!`:

 * No `panic!`s.  This includes checking for failing `assert!`s, since
   `assert!` calls `panic!` on failure.
 * No integer overflow, except when explicitly permitted via `wrapping_add` and
   similar methods.
 * No out-of-bounds array accesses.  This includes both Rust's normal bound
   checks (which `panic!` on failure) and also unchecked accesses through raw
   pointers.
 * No dereference of null or dangling pointers.  (Specifically, no dereference
   of pointers created by integer-to-pointer cast.)

`crux-mir` does not yet check for use-after-free in unsafe code.

