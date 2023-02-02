use std::ops::{Add, Sub};

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Vector {
  pub x: f64,
  pub y: f64,
  pub z: f64,
  pub w: f64,
}

impl std::fmt::Display for Vector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {}, {})", self.x, self.y, self.z)
    }
}

impl Add for Vector {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
          x: self.x + other.x,
          y: self.y + other.y,
          z: self.z + other.z,
          w: self.w + other.w,
        }
    }
}

impl Sub for Vector {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
          x: self.x - other.x,
          y: self.y - other.y,
          z: self.z - other.z,
          w: self.w - other.w,
        }
    }
}
