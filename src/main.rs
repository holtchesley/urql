#![feature(collections_range)]
#![feature(associated_type_defaults)]
#![feature(test)]
#![feature(iterator_step_by)]

#[cfg(test)]
extern crate quickcheck;

#[cfg(test)]
extern crate test;

extern crate itertools;

mod leapfrog;



trait Foo {
    type A:Ord;
    fn foo() {println!("Foo")}
}

trait Bar {
    type A:Ord;
    fn bar() {println!("Bar")}
}

trait FooBar :Foo+Bar {
    type A:Ord;
    fn foobar() {println!("FooBar")}
}




fn main() {
    println!("Hello, world!");
}
