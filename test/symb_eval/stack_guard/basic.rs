#![feature(custom_attribute)]

extern crate crucible;
use crucible::*;
use crucible::stack_guard::StackGuard;

#[crux_test]
fn crux_test() -> i32 {
    let mut x = 0;
    crucible_assert!(x == 0);
    {
        let _sg = StackGuard::new(|| { x += 1; });
        // NB: `x` normally would not be accessible here, but the borrow inside the cleanup closure
        // is hidden from the compiler.
        crucible_assert!(x == 0);
        // Cleanup function should happen here, incrementing `x`.
    }
    crucible_assert!(x == 1);
    x
}

pub fn main() {
    println!("{:?}", crux_test());
}
