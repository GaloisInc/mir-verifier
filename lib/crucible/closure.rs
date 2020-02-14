use core::mem;
use crate::any::Any;

/// `BoxFn<(A1, A2, ...), R>` is equivalent to `Box<dyn FnOnce(A1, A2, ...) -> R>`, but the latter
/// is not supported by mir-verifier at the moment.  (Specifically, it can't handle `Box<dyn Tr>`.)
pub struct BoxFn<A, R> {
    data: Any,
    invoke: fn(Any, A) -> R,
}

fn invoke<A, R, F: FnOnce<A, Output=R>>(f: Any, a: A) -> R {
    let f = unsafe { f.downcast::<F>() };
    f.call_once(a)
}

impl<A, R> BoxFn<A, R> {
    /// Box up a closure into a `BoxFn`.
    ///
    /// Note that any lifetimes inside `f` will be forgotten by the compiler.  This mostly works
    /// out okay thanks to mir-verifier's unusual memory model, but if `f` contains mutable
    /// borrows, mutation might happen in unexpected places, and might cause an assertion failure
    /// if it invalidates some other mutable reference.
    pub fn new<F: FnOnce<A, Output=R>>(f: F) -> BoxFn<A, R> {
        BoxFn {
            data: unsafe { Any::new_unchecked(f) },
            invoke: invoke::<A, R, F>,
        }
    }
}

impl<A, R> FnOnce<A> for BoxFn<A, R> {
    type Output = R;

    extern "rust-call" fn call_once(self, a: A) -> R {
        let r = (self.invoke)(self.data, a);
        mem::forget(self);
        r
    }
}
