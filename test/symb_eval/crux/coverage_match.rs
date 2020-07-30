extern crate crucible;
use crucible::*;

enum E { A, B, C }

#[cfg_attr(crux, crux_test)]
fn crux_test() -> i32 {
    let x = u8::symbolic("x");
    let e = match x {
        0 => E::A,
        1 => E::B,
        2 => E::C,
        _ => crucible_assume_unreachable!(),
    };
    crucible_assume!(x != 1);

    let y = match e {
        E::A => 1,
        E::B => 2,
        E::C => 3,
    };
    y
}
