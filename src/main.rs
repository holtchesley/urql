#![feature(collections_range)]
#![feature(associated_type_defaults)]
#![feature(test)]
// #![feature(rand)]
#![feature(iterator_step_by)]

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
extern crate rand;

#[cfg(test)]
extern crate test;


extern crate itertools;
extern crate chrono;
extern crate nom;


pub mod leapfrog;
pub mod parser;
pub mod compiler;
// pub mod unifier;

fn main() {
    println!("Hello, world!");
}
