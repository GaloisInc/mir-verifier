#![crate_type = "lib"]

#![stable(feature = "core", since = "1.6.0")]
#![no_core]

#![feature(allow_internal_unstable)]
#![feature(arbitrary_self_types)]
#![feature(asm)]
#![feature(associated_type_defaults)]
#![feature(cfg_target_has_atomic)]
#![feature(concat_idents)]
#![feature(const_fn)]
#![feature(const_fn_union)]
#![feature(custom_attribute)]
#![feature(doc_cfg)]
#![feature(doc_spotlight)]
#![feature(extern_types)]
#![feature(fundamental)]
#![feature(intrinsics)]
#![feature(lang_items)]
#![feature(link_llvm_intrinsics)]
#![feature(never_type)]
#![feature(nll)]
#![feature(exhaustive_patterns)]
#![feature(no_core)]
#![feature(on_unimplemented)]
#![feature(optin_builtin_traits)]
#![feature(prelude_import)]
#![feature(repr_simd, platform_intrinsics)]
#![feature(rustc_attrs)]
#![feature(rustc_const_unstable)]
#![feature(simd_ffi)]
#![feature(specialization)]
#![feature(staged_api)]
#![feature(stmt_expr_attributes)]
#![feature(unboxed_closures)]
#![feature(unsized_locals)]
#![feature(untagged_unions)]
#![feature(unwind_attributes)]
#![feature(doc_alias)]
#![feature(mmx_target_feature)]
#![feature(tbm_target_feature)]
#![feature(sse4a_target_feature)]
#![feature(arm_target_feature)]
#![feature(powerpc_target_feature)]
#![feature(mips_target_feature)]
#![feature(aarch64_target_feature)]
#![feature(wasm_target_feature)]
//#![feature(const_slice_len)]
//#![feature(const_str_as_bytes)]
//#![feature(const_str_len)]
#![feature(const_let)]
//#![feature(const_int_rotate)]
//#![feature(const_int_wrapping)]
//#![feature(const_int_sign)]
#![feature(const_int_conversion)]
#![feature(const_transmute)]
#![feature(reverse_bits)]
#![feature(non_exhaustive)]

#![feature(const_panic)]

#![feature(bind_by_move_pattern_guards)]
#![feature(structural_match)]
#![feature(abi_unadjusted)]
#![feature(external_doc)]
#![feature(core_intrinsics)]
#![feature(try_trait)]
//#![feature(pin)]
#![feature(coerce_unsized)]
#![feature(unsize)]
//#![feature(no_panic_pow)]
#![feature(wrapping_next_power_of_two)]
#![feature(allow_internal_unsafe)]
//#![feature(min_const_fn)]
#![feature(const_int_ops)]
#![feature(const_int_sign)]

#![allow(unused)]
#![allow(dead_code)]
#![allow(unused_attributes)]

#[prelude_import]
use prelude::v1::*;

#[macro_use] mod macros;
#[macro_use] mod internal_macros;

#[path = "num/int_macros.rs"]
#[macro_use]
mod int_macros;

#[path = "num/uint_macros.rs"]
#[macro_use]
mod uint_macros;

#[path = "num/isize.rs"] pub mod isize;
#[path = "num/i8.rs"]    pub mod i8;
#[path = "num/i16.rs"]   pub mod i16;
#[path = "num/i32.rs"]   pub mod i32;
#[path = "num/i64.rs"]   pub mod i64;
#[path = "num/i128.rs"]  pub mod i128;

#[path = "num/usize.rs"] pub mod usize;
#[path = "num/u8.rs"]    pub mod u8;
#[path = "num/u16.rs"]   pub mod u16;
#[path = "num/u32.rs"]   pub mod u32;
#[path = "num/u64.rs"]   pub mod u64;
#[path = "num/u128.rs"]  pub mod u128;

#[path = "num/f32.rs"]   pub mod f32;
#[path = "num/f64.rs"]   pub mod f64;

#[macro_use]
pub mod num;

/* The libcore prelude, not as all-encompassing as the libstd prelude */

pub mod prelude;

/* Core modules for ownership management */

pub mod intrinsics;
pub mod mem;
pub mod ptr;
pub mod hint;


/* Core language traits */

pub mod marker;
pub mod ops;
pub mod cmp;
pub mod clone;
pub mod default;
pub mod convert;
pub mod borrow;


/* Core types and methods on primitives */

pub mod cell;
pub mod panic;
pub mod panicking;
pub mod pin;
pub mod iter;
pub mod option;
pub mod result;


pub mod slice;

// note: does not need to be public
mod nonzero;
mod tuple;
mod unit;


