This rust cargo contains a single library `src/lib.rs` that is used to
define the standard library for mir-verifier. 

To be compatible with mir-lib, the libraries are derived from the
source code from commit 1cf82fd9c0 (i.e. the nightly version of rust
used by mir-json) of https://github.com/rust-lang/rust, in accordance
with the MIT LICENCE. The modules themselves are from
https://github.com/rust-lang/rust/tree/master/src/libcore,

This version of mir-lib is self-contained. It does not import anything
from `core`, only from other mir-lib modules. This is enabled with the
#[no_core] attribute at the top of src/lib.rs

(If mir-json is updated, then the libraries will also need to be
updated to the latest version. As there are still many manual edits to
these libraries that will be an onerous process. We need a more
automatic mechanism.)

==================================================================
* What is missing? How has the rust library source code been edited?

Some parts of the libraries are disabled via config attributes as they
rely on other parts that have not yet been included. In particular,
these added attributes include:

#[cfg(fmt)]   -- also remove deriving Debug
#[cfg(hash)]  -- also remove deriving Hash
#[cfg(ptr)]
#[cfg(cell)]
#[cfg(dec2flt)]
#[cfg(index)]
#[cfg(str)]
#[cfg(trusted_random_access)]
#[cfg(memchr)]
#[cfg(drop)]
#[cfg(any)]

The lowest level of pointer manipulation (i.e. ptr.rs) is *not*
included.  Operations that rely on these functions are edited to refer
to "intrinsics::abort()" instead. If these operations are needed by
the simulator, they should be made available via overrides.
(As well as any of the other intrinsics.)

==================================================================

How to add new pieces of libcore to mir-lib:

 - Add the new file to the crate through normal `mod` items.  If the
   new file needs features not already enabled, add the extra
   `#![feature]` attributes to `lib.rs`.

 - Leave any stability attributes intact.  Unfortunately, stability
   attributes are mandatory everywhere when `#![feature(staged_api)]`
   is enabled, and are forbidden everywhere when it's disabled.  (It
   seems easier to leave it enabled, since most code is copied from
   libcore and thus already has stability attributes.)





