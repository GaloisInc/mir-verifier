//! Types which pin data to its location in memory
//!
//! It is sometimes useful to have objects that are guaranteed to not move,
//! in the sense that their placement in memory does not change, and can thus be relied upon.
//!
//! A prime example of such a scenario would be building self-referencial structs,
//! since moving an object with pointers to itself will invalidate them,
//! which could cause undefined behavior.
//!
//! In order to prevent objects from moving, they must be pinned
//! by wrapping a pointer to the data in the [`Pin`] type. A pointer wrapped
//! in a `Pin` is otherwise equivalent to its normal version, e.g. `Pin<Box<T>>`
//! and `Box<T>` work the same way except that the first is pinning the value
//! of `T` in place.
//!
//! First of all, these are pointer types because pinned data mustn't be passed around by value
//! (that would change its location in memory).
//! Secondly, since data can be moved out of `&mut` and `Box` with functions such as [`swap`],
//! which causes their contents to swap places in memory,
//! we need dedicated types that prohibit such operations.
//!
//! However, these restrictions are usually not necessary,
//! so most types implement the [`Unpin`] auto-trait,
//! which indicates that the type can be moved out safely.
//! Doing so removes the limitations of pinning types,
//! making them the same as their non-pinning counterparts.
//!
//! [`Pin`]: struct.Pin.html
//! [`Unpin`]: trait.Unpin.html
//! [`swap`]: ../../std/mem/fn.swap.html
//! [`Box`]: ../../std/boxed/struct.Box.html
//!
//! # Examples
//!
//! ```rust
//! #![feature(pin)]
//!
//! use std::pin::Pin;
//! use std::marker::Pinned;
//! use std::ptr::NonNull;
//!
//! // This is a self referencial struct since the slice field points to the data field.
//! // We cannot inform the compiler about that with a normal reference,
//! // since this pattern cannot be described with the usual borrowing rules.
//! // Instead we use a raw pointer, though one which is known to not be null,
//! // since we know it's pointing at the string.
//! struct Unmovable {
//!     data: String,
//!     slice: NonNull<String>,
//!     _pin: Pinned,
//! }
//!
//! impl Unmovable {
//!     // To ensure the data doesn't move when the function returns,
//!     // we place it in the heap where it will stay for the lifetime of the object,
//!     // and the only way to access it would be through a pointer to it.
//!     fn new(data: String) -> Pin<Box<Self>> {
//!         let res = Unmovable {
//!             data,
//!             // we only create the pointer once the data is in place
//!             // otherwise it will have already moved before we even started
//!             slice: NonNull::dangling(),
//!             _pin: Pinned,
//!         };
//!         let mut boxed = Box::pinned(res);
//!
//!         let slice = NonNull::from(&boxed.data);
//!         // we know this is safe because modifying a field doesn't move the whole struct
//!         unsafe {
//!             let mut_ref: Pin<&mut Self> = Pin::as_mut(&mut boxed);
//!             Pin::get_mut_unchecked(mut_ref).slice = slice;
//!         }
//!         boxed
//!     }
//! }
//!
//! let unmoved = Unmovable::new("hello".to_string());
//! // The pointer should point to the correct location,
//! // so long as the struct hasn't moved.
//! // Meanwhile, we are free to move the pointer around.
//! # #[allow(unused_mut)]
//! let mut still_unmoved = unmoved;
//! assert_eq!(still_unmoved.slice, NonNull::from(&still_unmoved.data));
//!
//! // Since our type doesn't implement Unpin, this will fail to compile:
//! // let new_unmoved = Unmovable::new("world".to_string());
//! // std::mem::swap(&mut *still_unmoved, &mut *new_unmoved);
//! ```

#![unstable(feature = "pin", issue = "49150")]

use intrinsics;

#[cfg(fmt)]
use fmt;
use marker::Sized;
use ops::{Deref, DerefMut, CoerceUnsized};

#[doc(inline)]
pub use marker::Unpin;

/// A pinned pointer.
///
/// This is a wrapper around a kind of pointer which makes that pointer "pin" its
/// value in place, preventing the value referenced by that pointer from being moved
/// unless it implements [`Unpin`].
///
/// See the [`pin` module] documentation for further explanation on pinning.
///
/// [`Unpin`]: ../../std/marker/trait.Unpin.html
/// [`pin` module]: ../../std/pin/index.html
//
// Note: the derives below are allowed because they all only use `&P`, so they
// cannot move the value behind `pointer`.
#[unstable(feature = "pin", issue = "49150")]
#[fundamental]
//#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Pin<P> {
    pointer: P,
}

impl<P: Deref> Pin<P>
where
    P::Target: Unpin,
{
    /// Construct a new `Pin` around a pointer to some data of a type that
    /// implements `Unpin`.
    #[unstable(feature = "pin", issue = "49150")]
    #[inline(always)]
    pub fn new(pointer: P) -> Pin<P> {
        // Safety: the value pointed to is `Unpin`, and so has no requirements
        // around pinning.
        unsafe { intrinsics::abort() }  
//        unsafe { Pin::new_unchecked(pointer) }
    }
}

impl<P: Deref> Pin<P> {
    /// Construct a new `Pin` around a reference to some data of a type that
    /// may or may not implement `Unpin`.
    ///
    /// # Safety
    ///
    /// This constructor is unsafe because we cannot guarantee that the data
    /// pointed to by `pointer` is pinned. If the constructed `Pin<P>` does
    /// not guarantee that the data `P` points to is pinned, constructing a
    /// `Pin<P>` is undefined behavior.
    ///
    /// If `pointer` dereferences to an `Unpin` type, `Pin::new` should be used
    /// instead.
    #[unstable(feature = "pin", issue = "49150")]
    #[inline(always)]
    pub unsafe fn new_unchecked(pointer: P) -> Pin<P> {
        Pin { pointer }
    }

    /// Get a pinned shared reference from this pinned pointer.
    #[unstable(feature = "pin", issue = "49150")]
    #[inline(always)]
    pub fn as_ref(self: &Pin<P>) -> Pin<&P::Target> {
        unsafe { intrinsics::abort() }          
        // unsafe { Pin::new_unchecked(&*self.pointer) }
    }
}

impl<P: DerefMut> Pin<P> {
    /// Get a pinned mutable reference from this pinned pointer.
    #[unstable(feature = "pin", issue = "49150")]
    #[inline(always)]
    pub fn as_mut(self: &mut Pin<P>) -> Pin<&mut P::Target> {
        unsafe { intrinsics::abort() }  
//        unsafe { Pin::new_unchecked(&mut *self.pointer) }
    }

    /// Assign a new value to the memory behind the pinned reference.
    #[unstable(feature = "pin", issue = "49150")]
    #[inline(always)]
    pub fn set(mut self: Pin<P>, value: P::Target)
    where
        P::Target: Sized,
    {
        unsafe { intrinsics::abort() }          
        //*self.pointer = value;
    }
}

impl<'a, T: ?Sized> Pin<&'a T> {
    /// Construct a new pin by mapping the interior value.
    ///
    /// For example, if you  wanted to get a `Pin` of a field of something,
    /// you could use this to get access to that field in one line of code.
    ///
    /// # Safety
    ///
    /// This function is unsafe. You must guarantee that the data you return
    /// will not move so long as the argument value does not move (for example,
    /// because it is one of the fields of that value), and also that you do
    /// not move out of the argument you receive to the interior function.
    #[unstable(feature = "pin", issue = "49150")]
    pub unsafe fn map_unchecked<U, F>(this: Pin<&'a T>, func: F) -> Pin<&'a U> where
        F: FnOnce(&T) -> &U,
    {
        let pointer = &*this.pointer;
        let new_pointer = func(pointer);
        Pin::new_unchecked(new_pointer)
    }

    /// Get a shared reference out of a pin.
    ///
    /// Note: `Pin` also implements `Deref` to the target, which can be used
    /// to access the inner value. However, `Deref` only provides a reference
    /// that lives for as long as the borrow of the `Pin`, not the lifetime of
    /// the `Pin` itself. This method allows turning the `Pin` into a reference
    /// with the same lifetime as the original `Pin`.
    #[unstable(feature = "pin", issue = "49150")]
    #[inline(always)]
    pub fn get_ref(this: Pin<&'a T>) -> &'a T {
        this.pointer
    }
}

impl<'a, T: ?Sized> Pin<&'a mut T> {
    /// Convert this `Pin<&mut T>` into a `Pin<&T>` with the same lifetime.
    #[unstable(feature = "pin", issue = "49150")]
    #[inline(always)]
    pub fn into_ref(this: Pin<&'a mut T>) -> Pin<&'a T> {
        Pin { pointer: this.pointer }
    }

    /// Get a mutable reference to the data inside of this `Pin`.
    ///
    /// This requires that the data inside this `Pin` is `Unpin`.
    ///
    /// Note: `Pin` also implements `DerefMut` to the data, which can be used
    /// to access the inner value. However, `DerefMut` only provides a reference
    /// that lives for as long as the borrow of the `Pin`, not the lifetime of
    /// the `Pin` itself. This method allows turning the `Pin` into a reference
    /// with the same lifetime as the original `Pin`.
    #[unstable(feature = "pin", issue = "49150")]
    #[inline(always)]
    pub fn get_mut(this: Pin<&'a mut T>) -> &'a mut T
        where T: Unpin,
    {
        this.pointer
    }

    /// Get a mutable reference to the data inside of this `Pin`.
    ///
    /// # Safety
    ///
    /// This function is unsafe. You must guarantee that you will never move
    /// the data out of the mutable reference you receive when you call this
    /// function, so that the invariants on the `Pin` type can be upheld.
    ///
    /// If the underlying data is `Unpin`, `Pin::get_mut` should be used
    /// instead.
    #[unstable(feature = "pin", issue = "49150")]
    #[inline(always)]
    pub unsafe fn get_mut_unchecked(this: Pin<&'a mut T>) -> &'a mut T {
        this.pointer
    }

    /// Construct a new pin by mapping the interior value.
    ///
    /// For example, if you  wanted to get a `Pin` of a field of something,
    /// you could use this to get access to that field in one line of code.
    ///
    /// # Safety
    ///
    /// This function is unsafe. You must guarantee that the data you return
    /// will not move so long as the argument value does not move (for example,
    /// because it is one of the fields of that value), and also that you do
    /// not move out of the argument you receive to the interior function.
    #[unstable(feature = "pin", issue = "49150")]
    pub unsafe fn map_unchecked_mut<U, F>(this: Pin<&'a mut T>, func: F) -> Pin<&'a mut U> where
        F: FnOnce(&mut T) -> &mut U,
    {
        let pointer = Pin::get_mut_unchecked(this);
        let new_pointer = func(pointer);
        Pin::new_unchecked(new_pointer)
    }
}

#[unstable(feature = "pin", issue = "49150")]
impl<P: Deref> Deref for Pin<P> {
    type Target = P::Target;
    fn deref(&self) -> &P::Target {
        Pin::get_ref(Pin::as_ref(self))
    }
}

#[unstable(feature = "pin", issue = "49150")]
impl<P: DerefMut> DerefMut for Pin<P>
where
    P::Target: Unpin
{
    fn deref_mut(&mut self) -> &mut P::Target {
        Pin::get_mut(Pin::as_mut(self))
    }
}

#[cfg(fmt)]
#[unstable(feature = "pin", issue = "49150")]
impl<P: fmt::Debug> fmt::Debug for Pin<P> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.pointer, f)
    }
}

#[cfg(fmt)]
#[unstable(feature = "pin", issue = "49150")]
impl<P: fmt::Display> fmt::Display for Pin<P> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.pointer, f)
    }
}

#[cfg(fmt)]
#[unstable(feature = "pin", issue = "49150")]
impl<P: fmt::Pointer> fmt::Pointer for Pin<P> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Pointer::fmt(&self.pointer, f)
    }
}

// Note: this means that any impl of `CoerceUnsized` that allows coercing from
// a type that impls `Deref<Target=impl !Unpin>` to a type that impls
// `Deref<Target=Unpin>` is unsound. Any such impl would probably be unsound
// for other reasons, though, so we just need to take care not to allow such
// impls to land in std.
#[unstable(feature = "pin", issue = "49150")]
impl<P, U> CoerceUnsized<Pin<U>> for Pin<P>
where
    P: CoerceUnsized<U>,
{}

#[unstable(feature = "pin", issue = "49150")]
impl<P> Unpin for Pin<P> {}
