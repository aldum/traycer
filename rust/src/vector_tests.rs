#[cfg(test)]
mod vector_tests {
    use crate::vector::Vector;

    #[test]
    fn t1() {
        let v1 = Vector {
            x: 1.0, y: 2.0, z: 3.0, w: 0.0
        };
        let v2 = Vector{
            x: 2.0, y: 4.0, z: 0.0, w: 1.0
        };

        let result = Vector{
            x: 3.0, y: 6.0, z: 3.0, w: 1.0
        };
        assert_eq!(v1 + v2, result);
    }
}