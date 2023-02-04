use std::{ops::{Add, Sub, Mul, Neg, Rem}, f64::EPSILON};

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

    pub fn length(&self) -> f64 { 
        (self.x * self.x + 
            self.y * self.y +
            self.z * self.z).sqrt()
    }

    pub fn normalized(&self) -> Tuple { 
        let l = self.length();
        if l < EPSILON { 
            Tuple { ..*self }
        } else {
            Tuple {
                x: self.x / l,
                y: self.y / l,
                z: self.z / l,
                ..*self
            }
        }
  }
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


impl Mul<f64> for Tuple {
    type Output = Self;

    fn mul(self, s: f64) -> Self {
        Self {
            x: self.x * s,
            y: self.y * s,
            z: self.z * s,
            w: self.w,
        }
    }
}

impl Mul<Tuple> for f64 {
    type Output = Tuple;

    fn mul(self, t: Tuple) -> Self::Output {
        Tuple {
            x: t.x * self,
            y: t.y * self,
            z: t.z * self,
            w: t.w
        }
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

impl Neg for Tuple {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self {
            x: -1.0 * self.x,
            y: -1.0 * self.y,
            z: -1.0 * self.z,
            w: self.w
        }
    }
    
}

impl Mul<Tuple> for Tuple {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        Self {
            x: self.x * other.x,
            y: self.y * other.y,
            z: self.z * other.z,
            w: self.w,
        }
    }
}

impl Rem<Tuple> for Tuple {
    type Output = Self;

    fn rem(self, other: Tuple) -> Self::Output {
        Self {
            x: (self.y * other.z) - (self.z * other.y),
            y: (self.z * other.x) - (self.x * other.z),
            z: (self.x * other.y) - (self.y * other.x),
            w: self.w,
        }
    }

}

