pub enum O<T> {
    N,
    S(T),
}

use self::O::*;

impl<T> O<T> {
  
    pub fn as_mut(&mut self) -> O<&mut T> {
        match *self {
            S(ref mut x) => S(x),
            N => N,
        }
    }
    
}

pub fn g(mut x:O<i32>) -> O<i32> {
    x =
        match x {
            S(y) => S(y+1),
            N    => x
        };
    x      
}


pub fn f (x:i32) -> i32 {
    let z = S(x);

    //let z = z.as_mut();
    match z {
        S(x) => x,
        N => 5
    }
}


const ARG: i32 = 14;

#[cfg(with_main)]
fn main() {
   println!("{:?}", f(ARG));
}