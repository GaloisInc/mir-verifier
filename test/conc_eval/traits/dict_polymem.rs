// FAIL: Bad any unpack due to polymorphism in dictionary
// a static trait invocation for a polymorphic member 
// calling the g method in h requires a dictionary argument 


trait G {
    fn g<T>(&self,T) -> T;
//    fn w<T:G> (&self,T) -> T;
}

impl G for u32 {
    fn g<T>(&self,x:T) -> T {
        return x
    }
//    fn w<T:G>(&self,x:T) -> T {
//        return self.g(x)
//    }
}

fn h<T>(x:T,y:T) -> T where T:G {
    x.g(y)
}

fn f(_: ()) -> u32 {
    let d : u32 = 5;
    let e : u32 = 4;
    h(d,e)
}

const ARG: () = ();

#[cfg(with_main)]
fn main() {
   println!("{:?}", f(ARG));
}
