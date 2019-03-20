fn f(_x : u16) -> bool {
    let y : u16 = 20;
    let z : i16 = 20;
    
    let mut res : bool = true;
    
    res = res && y.wrapping_sub(22) == 65534;
    res = res && y.wrapping_sub(18) == 2;
    res = res && z.wrapping_sub(22) == 65534;
    res = res && z.wrapping_sub(18) == 2;

    res
}

const ARG : u16 = 20;

#[cfg(with_main)]
fn main() {
   println!("{:?}", f(ARG));
}
