#![no_std]
extern crate std;
#[macro_use] extern crate crucible;
use crucible::*;

use std::io::{Cursor, Read, Write};

pub fn f(x: u8) -> u8 {
    let mut buf = [0; 5];
    let msg = b"hello, world!" as &[u8];

    let mut c = Cursor::new(msg);
    c.read(&mut buf);

    for (a, b) in buf.iter().zip(msg.iter()) {
        crucible_assert!(a == b);
    }

    0
}

pub static ARG: u8 = 1;

#[cfg(with_main)] pub fn main() { println!("{:?}", f(ARG)); }
#[cfg(not(with_main))] pub fn main() { f(ARG); }