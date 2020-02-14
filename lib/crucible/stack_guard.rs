//! Implements `StackGuard`, which invokes a closure when it's dropped.  We don't have full drop
//! support in mir-json or mir-verifier, so every part of this module is a terrible hack.  The main
//! design constraint here is to minimize the number and complexity of special cases required in
//! mir-verifier's translation.  Currently there is only one: `TerminatorKind::Drop` of a value of
//! type `StackGuard` will invoke `stack_guard_cleanup`.
//!
//! The only supported way to use this type is:
//!
//!     let _g = StackGuard::new(|| /* your code here */);
//!
//! and then never move or otherwise access `_g`.  Any kind of funny business, like moving
//! `StackGuard`s around or taking them as arguments, will likely result in your cleanup code not
//! being called.
//!
//! Note that any borrows held by the closure will be hidden from the borrow checker.  Normally
//! this could result in use-after-frees, but due to mir-verifier's unusual memory model, instead
//! it can only result in mutations happening at surprising times.

use core::mem;
use crate::closure::BoxFn;


pub struct StackGuard(BoxFn<(), ()>);

impl StackGuard {
    pub fn new<F: FnOnce()>(f: F) -> StackGuard {
        // Make sure `stack_guard_cleanup` is included in any crate that uses `StackGuard::new`.
        let _require_cleanup = stack_guard_cleanup;
        StackGuard(BoxFn::new(f))
    }

    pub fn cleanup(self) {
        // `&T` and `T` have the same representation.  We use this to work around the restriction
        // on moving out of a type that implements `Drop`.
        let f = unsafe {
            mem::crucible_identity_transmute::<&BoxFn<(), ()>, BoxFn<(), ()>>(&self.0)
        };
        f();
        // Avoid dropping `self` at the end of the function, to avoid infinite recursion via
        // `TerminatorKind::Drop` / `stack_guard_cleanup`.
        mem::forget(self);
    }
}

// `StackGuard` must have a `Drop` impl, or else `TerminatorKind::Drop` won't be generated for
// `StackGuard` locals.
impl Drop for StackGuard {
    fn drop(&mut self) {
        // `drop` should never be reached.  Our current limited drop support performs special
        // translation of the `Drop` terminator for `StackGuard`, and never generates calls to
        // `Drop::drop` impls.
        unimplemented!("StackGuard::drop should never be called")
    }
}

fn stack_guard_cleanup(sg: StackGuard) {
    sg.cleanup();
}
