#[cfg(test)]
mod geometry_tests {
    use crate::geometry::Tuple;


    #[test]
    fn t1() {
        let v1 = Tuple::vector(1.0, 2.0, 3.0);
        let v2 = Tuple::vector(2.0, 4.0, 0.0);

        let result = Tuple::vector(3.0, 6.0, 3.0);
        assert_eq!(v1 + v2, result);
    }

    #[test]
    fn t_scalar_mult() {
        let v1 = Tuple::vector(1.0, 2.0, 3.0);

        let result = Tuple::vector(2.0, 4.0, 6.0);
        assert_eq!(v1 * 2.0, result);
        assert_eq!(2.0 * v1 , result);
    }
}
