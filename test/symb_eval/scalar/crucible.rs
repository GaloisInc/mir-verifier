pub fn crucible_u64(_x: &'static str) -> u64 { unimplemented!() }

macro_rules! crucible_scalar64 {
    ($desc:expr) => {
        Scalar52([
            $crate::crucible::crucible_u64(concat!($desc, "_0")),
            $crate::crucible::crucible_u64(concat!($desc, "_1")),
            $crate::crucible::crucible_u64(concat!($desc, "_2")),
            $crate::crucible::crucible_u64(concat!($desc, "_3")),
            $crate::crucible::crucible_u64(concat!($desc, "_4")),
        ])
    };
}

#[allow(unused_variables)]
pub fn crucible_assert_impl(
    cond: bool,
    cond_str: &'static str,
    file: &'static str,
    line: u32,
    col: u32,
) -> () {
    ()
}

#[allow(unused_variables)]
pub fn crucible_assume_impl(
    cond: bool,
    cond_str: &'static str,
    file: &'static str,
    line: u32,
    col: u32,
) -> () {
    ()
}

macro_rules! crucible_assert {
    ($e:expr) => {
        $crate::crucible::crucible_assert_impl($e, stringify!($e), file!(), line!(), column!())
    };
}

macro_rules! crucible_assume {
    ($e:expr) => {
        $crate::crucible::crucible_assume_impl($e, stringify!($e), file!(), line!(), column!())
    };
}

macro_rules! crucible_debug_integer {
    ($e:expr) => { crucible_debug_integer!($e, stringify!($e)) };
    ($e:expr, $desc:expr) => {
        crucible_assume!($e == $crate::int512::Int512::symbolic($desc))
    };
}
