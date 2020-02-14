// FAIL: Box<dyn> is not yet supported
#![cfg_attr(not(with_main), feature(custom_attribute))]

pub fn call_it(f: Box<FnOnce(i32) -> i32>) -> i32 {
    f(1)
}

pub fn f(x: i32) -> i32 {
    let mut z = 0;
    let a = call_it(Box::new(move |y| x + y));
    let b = call_it(Box::new(move |y| x + y));
    a + b
}

pub static ARG: i32 = 1;

#[cfg(with_main)] pub fn main() { println!("{:?}", f(ARG)); }
#[cfg(not(with_main))] #[crux_test] fn crux_test() -> i32 { f(ARG) }
