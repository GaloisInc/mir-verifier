// FAIL: function pointers in consts are not handled by mir-json
#![cfg_attr(not(with_main), no_std)]
#![cfg_attr(not(with_main), feature(custom_attribute))]

fn f() -> i32 { 1 }
fn g() -> i32 { 2 }

const FN_PTR: fn() -> i32 = f;

#[cfg(with_main)]
pub fn main() {
    println!("{:?}", FN_PTR());
}
#[cfg(not(with_main))] #[crux_test] fn crux_test() -> i32 { FN_PTR() }