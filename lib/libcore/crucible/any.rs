/// Dynamically-typed wrapper, corresponding to Crucible's `AnyType`.
#[unstable(feature = "crucible_intrinsics", issue = "0")]
#[derive(Clone, Copy)]
pub struct Any(());

impl Any {
    /// Wrap an arbitrary value in `Any`.
    #[unstable(feature = "crucible_intrinsics", issue = "0")]
    pub fn new<T: Copy>(x: T) -> Any {
        unsafe { Self::new_unchecked(x) }
    }

    /// Wrap an arbitrary value in `Any` without requiring `Copy`.  If `T` is not `Copy`, then the
    /// caller must ensure that the `Any` value is only `downcast` once - otherwise, `x` may be
    /// copied despite not supporting it.
    #[unstable(feature = "crucible_intrinsics", issue = "0")]
    pub unsafe fn new_unchecked<T>(x: T) -> Any {
        unimplemented!("Any::new_unchecked")
    }

    /// Try to downcast to concrete type `T`.  This succeeds if the type passed to `new` has the
    /// same Crucible representation as the type passed to `downcast` (similar to the condition on
    /// `crucible_identity_transmute`).  There is not actually any way to check for an exact type
    /// match at the Rust level.
    ///
    /// This function is unsafe because `new` + `downcast` is equivalent to
    /// `crucible_identity_transmute`.
    #[unstable(feature = "crucible_intrinsics", issue = "0")]
    pub unsafe fn downcast<T>(self) -> T {
        unimplemented!("Any::downcast")
    }
}

