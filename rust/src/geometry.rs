use std::ops::{Add, Sub};


#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Tuple {
  pub x: f64,
  pub y: f64,
  pub z: f64,
  w: f64,
}

impl Tuple {
  pub fn point(x: f64, y: f64, z: f64) -> Tuple {
    Tuple{x, y, z, w: 1.0}
  }
  pub fn vector(x: f64, y: f64, z: f64) -> Tuple {
    Tuple{x, y, z, w: 0.0}
  }
  pub fn is_point(&self) -> bool { self.w == 1.0 }
  pub fn is_vector(&self) -> bool { self.w == 0.0 }
}


impl std::fmt::Display for Tuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let l = if self.is_point() {
          "Point" } else if self.is_vector() {
          "Vector"
        } else { "" };
        write!(f, "{}({}, {}, {})", l, self.x, self.y, self.z)
    }
}

impl Add for Tuple {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
          x: self.x + other.x,
          y: self.y + other.y,
          z: self.z + other.z,
          w: self.w//  + other.w,
        }
    }
}

impl Sub for Tuple {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
          x: self.x - other.x,
          y: self.y - other.y,
          z: self.z - other.z,
          w: self.w// - other.w,
        }
    }
}
