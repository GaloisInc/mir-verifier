// FAIL: function pointers in consts are not handled by mir-json
#![cfg_attr(not(with_main), no_std)]
#![cfg_attr(not(with_main), feature(custom_attribute))]

struct LocalKey {
    fn_ptr: fn() -> i32,
}

impl LocalKey {
    fn get(&self) -> i32 {
        (self.fn_ptr)()
    }
}

fn f() -> i32 { 1 }
fn g() -> i32 { 2 }

const LOCAL_KEY: LocalKey = LocalKey { fn_ptr: f };

#[cfg(with_main)]
pub fn main() {
    println!("{:?}", LOCAL_KEY.get());
}
#[cfg(not(with_main))] #[crux_test] fn crux_test() -> i32 { LOCAL_KEY.get() }
