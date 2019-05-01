// FAIL: promoted lvalue

fn k(x : u32) -> u32 {
    x + 1
}

fn app<G>(x : &G, y : u32) -> u32
  where G : Fn(u32) -> u32 {
    x(y)
}


fn f(_: ()) -> u32 {
    let d = 32;
    app(&k,d)
}

const ARG: () = ();

#[cfg(with_main)]
fn main() {
   println!("{:?}", f(ARG));
}