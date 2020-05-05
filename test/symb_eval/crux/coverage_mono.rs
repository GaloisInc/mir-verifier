use std::mem;
extern crate crucible;
use crucible::*;

fn f<T>(c: bool, t: T, e: T) -> T {
    if c { t } else { e }
}

#[cfg_attr(crux, crux_test)]
fn crux_test() -> i32 {
    f(false, 1_u8, 2_u8) as i32 + f(true, 10, 20)
}
