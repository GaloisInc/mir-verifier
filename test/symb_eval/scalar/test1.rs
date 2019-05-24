extern crate core;

#[macro_use] mod crucible;
mod scalar;
mod constants;
mod int512;

use scalar::Scalar52;
use int512::Int512;


// Conversions: Scalar52 <-> Int512

impl From<Scalar52> for Int512 {
    fn from(x: Scalar52) -> Int512 {
        let mut acc = Int512::from(0_i32);
        for i in 0..5 {
            acc = acc | (Int512::from(x.0[i]) << (52 * i as u32));
        }
        acc
    }
}

impl From<Int512> for Scalar52 {
    fn from(x: Int512) -> Scalar52 {
        let mut acc = Scalar52([0; 5]);
        let mask: Int512 = Int512::from((1_u64 << 52) - 1);
        for i in 0..5 {
            acc.0[i] = u64::from((x >> (52 * i as u32)) & mask);
        }
        acc
    }
}


// Conversions: Int512 <-> bytes

impl From<[u8; 32]> for Int512 {
    fn from(x: [u8; 32]) -> Int512 {
        let mut acc = Int512::from(0_i32);
        for i in 0..5 {
            acc = acc | (Int512::from(x[i]) << (8 * i as u32));
        }
        acc
    }
}

impl From<Int512> for [u8; 32] {
    fn from(x: Int512) -> [u8; 32] {
        let mut acc = [0; 32];
        let mask: Int512 = Int512::from((1_u64 << 8) - 1);
        for i in 0..32 {
            acc[i] = u8::from((x >> 8 * i as u32) & mask);
        }
        acc
    }
}


/// `constants::L` (the modulus), represented as an `Int512`.
pub fn L() -> Int512 {
    (Int512::from(1_i32) << 252) +
        (Int512::from(0x5812631a5cf5d3ed_u64) | (Int512::from(0x14def9dea2f79cd6_u64) << 64))
}

/// Check if an integer is in the range `0 .. L`.
pub fn is_valid(x: Int512) -> bool {
    Int512::from(0_i32) <= x && x < L()
}


fn f(_w : u64 ) -> bool {
    // Int512 -> Scalar52 -> Int512 conversion is the identity function.
    {
        let i = Int512::symbolic("int_to_from_scalar");
        crucible_assume!(is_valid(i));
        let s = Scalar52::from(i);
        let i2 = Int512::from(s);
        crucible_assert!(i2 == i);
    }

    // Scalar52 -> Int512 -> Scalar52 conversion is the identity function.
    {
        let s = crucible_scalar64!("scalar_to_from_int");
        for i in 0..5 {
            crucible_assume!(s.0[i] < 1 << 52);
        }
        let i = Int512::from(s);
        let s2 = Scalar52::from(i);
        for i in 0..5 {
            crucible_assert!(s2.0[i] == s.0[i]);
        }
    }

    // Serialization is correct: Scalar52 -> [u8] -> Scalar52 is the identity.
    {
        let i = Int512::symbolic("scalar_to_from_bytes");
        crucible_assume!(is_valid(i));
        let s = Scalar52::from(i);
        let b = s.to_bytes();
        let s2 = Scalar52::from_bytes(&b);
        for i in 0..5 {
            crucible_assert!(s2.0[i] == s.0[i]);
        }
    }

    // [u8] ->  Scalar52 -> [u8] is the identity.
    {
        let i = Int512::symbolic("scalar_from_to_bytes");
        crucible_assume!(is_valid(i));
        let b = <[u8; 32]>::from(i);
        let s = Scalar52::from_bytes(&b);
        let b2 = s.to_bytes();
        for i in 0..32 {
            crucible_assert!(b2[i] == b[i]);
        }
    }

    // Serialization uses the little-endian format defined in Int512 -> [u8]: the two conversions
    // Int512 -> Scalar52 -> [u8] and Int512 -> [u8] produce identical results.
    {
        let i = Int512::symbolic("scalar_int_to_bytes");
        crucible_assume!(is_valid(i));
        let s = Scalar52::from(i);
        let b = s.to_bytes();
        let b2 = <[u8; 32]>::from(i);
        for i in 0..32 {
            crucible_assert!(b2[i] == b[i]);
        }
    }

    // Scalar52::add is a correct implemnetation of addition modulo L: Int512 -> Scalar52 +
    // Scalar52::add + Scalar52 -> Int512 produces the same result as using Int512::add +
    // Int512::rem directly.
    {
        let a = Int512::symbolic("add_a");
        let b = Int512::symbolic("add_b");
        crucible_assume!(is_valid(a));
        crucible_assume!(is_valid(b));

        let s_a = Scalar52::from(a);
        let s_b = Scalar52::from(b);
        let s_y = Scalar52::add(&s_a, &s_b);

        let y = Int512::from(s_y);
        // We'd like to say `y == (a + b) % L`, but the solver can't handle it (times out).
        // Instead, we assert a slightly stronger claim, which does not use division:
        //      1. 0 <= y < L
        //      2. y == a + b  OR  y == a + b - L
        // This implies the original claim because we already know 0 <= a + b < 2 * L.
        crucible_assert!(is_valid(y));
        crucible_assert!(y == a + b || y == a + b - L());
    }

    return true;
}

const ARG: u64 = 20;
#[cfg(with_main)]
fn main() {
   println!("{:?}", f(ARG));
}
