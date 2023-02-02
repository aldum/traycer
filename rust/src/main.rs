use crate::geometry::Tuple;

pub mod geometry;
pub mod vector_tests;
// use vector::Vector;

fn main() {
    println!("Hello, world!");

    println!("{}", Tuple::vector(1.0, 2.0, 3.0));
    println!("{}", Tuple::point(1.0, 2.0, 3.0));
}
