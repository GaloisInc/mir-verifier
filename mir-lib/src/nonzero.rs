// Copyright 2012-2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Exposes the NonZero lang item which provides optimization hints.

use ops::CoerceUnsized;

/// A wrapper type for raw pointers and integers that will never be
/// NULL or 0 that might allow certain optimizations.
#[rustc_layout_scalar_valid_range_start(1)]
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
//#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub(crate) struct NonZero<T>(pub(crate) T);

impl<T: CoerceUnsized<U>, U> CoerceUnsized<NonZero<U>> for NonZero<T> {}
