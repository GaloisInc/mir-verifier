pub enum Opt<T> {
    /// No value
    No,
    /// Some value `T`
    So(T)
}

use Opt::*;
    


trait Foo {
    fn method(&self) -> i32;
    fn static_method() -> i32;
    fn default_method(&self) -> i32 {
        self.method() + Self::static_method()
    }
}

trait Foo2 {
    type Assoc;
    fn take_assoc(&self, a: Self::Assoc) -> i32;
    fn give_assoc(&self, a: i32) -> Self::Assoc;
    fn default_with_assoc(&self, a: i32) -> i32 {
        self.take_assoc(self.give_assoc(a))
    }
}

#[derive(Clone, Copy)]
struct S;

impl Foo for S {
    fn method(&self) -> i32 { 1 }
    fn static_method() -> i32 { 2 }
}

impl Foo2 for S {
    type Assoc = i32;

    fn take_assoc(&self, a: Self::Assoc) -> i32 { a }
    fn give_assoc(&self, a: i32) -> Self::Assoc { a }
}


impl<T: Foo> Foo2 for Opt<T> {
    type Assoc = i32;
    fn take_assoc(&self, a: Self::Assoc) -> i32 { a }
    fn give_assoc(&self, a: i32) -> Self::Assoc {
        if let So(ref x) = *self {
            a + x.method() - 1
        } else {
            a
        }
    }
}


const ARG: i32 = 1;
fn f(arg: i32) {
    let some_s = So(S);
    assert!(some_s.take_assoc(1) == 1);
    assert!(some_s.give_assoc(1) == 1);
    assert!(some_s.default_with_assoc(1) == 1);
}

#[cfg(with_main)]
fn main() {
   println!("{:?}", f(ARG));
}
