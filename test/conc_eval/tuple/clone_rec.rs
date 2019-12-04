#![feature(custom_attribute)]

#[crux_test]
pub fn f() {
    let x = (1, (2, (3, 4)));
    let y = x.clone();
    assert!(x == y);
}

#[cfg(with_main)] pub fn main() { println!("{:?}", f()); }