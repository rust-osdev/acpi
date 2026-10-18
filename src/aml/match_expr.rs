use crate::aml::object::Package;

const ONES: u64 = !0;

pub fn do_match(
    search_package: &Package,
    op_a: &MatchOp,
    operand_a: u64,
    op_b: &MatchOp,
    operand_b: u64,
    start_index: u64,
) -> u64 {
    for (idx, row) in search_package[start_index as usize..].iter().enumerate() {
        let Ok(v) = row.as_integer() else {
            continue;
        };

        if match_comparison(v, op_a, operand_a) && match_comparison(v, op_b, operand_b) {
            return start_index + idx as u64;
        }
    }

    ONES
}

fn match_comparison(value: u64, op: &MatchOp, operand: u64) -> bool {
    match op {
        MatchOp::MTR => true,
        MatchOp::MEQ => value == operand,
        MatchOp::MLE => value <= operand,
        MatchOp::MLT => value < operand,
        MatchOp::MGE => value >= operand,
        MatchOp::MGT => value > operand,
    }
}

#[allow(clippy::upper_case_acronyms)] // Allow the same capitalisation as the spec.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum MatchOp {
    MTR = 0,
    MEQ = 1,
    MLE = 2,
    MLT = 3,
    MGE = 4,
    MGT = 5,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::aml::object::Object;
    use std::string::ToString;

    #[test]
    fn empty_package_doesnt_match() {
        assert_eq!(ONES, do_match(&vec![], &MatchOp::MEQ, 0, &MatchOp::MTR, 0, 0));
    }

    #[test]
    fn basic_match() {
        let pkg = &([0, 1, 2, 3].iter().map(|i| Object::Integer(*i as u64).wrap()).collect());
        assert_eq!(2, do_match(pkg, &MatchOp::MEQ, 2, &MatchOp::MTR, 0, 0));
    }

    #[test]
    fn skips_strings() {
        let pkg = [
            Object::Integer(0),
            Object::String("1".to_string()),
            Object::String("2".to_string()),
            Object::Integer(3),
        ]
        .into_iter()
        .map(|o| o.wrap())
        .collect();

        assert_eq!(3, do_match(&pkg, &MatchOp::MGT, 2, &MatchOp::MTR, 0, 0));
    }

    #[test]
    fn skips_packages() {
        let pkg = [Object::Integer(0), Object::Package(vec![Object::Integer(1).wrap()]), Object::Integer(2)]
            .into_iter()
            .map(|o| o.wrap())
            .collect();

        assert_eq!(2, do_match(&pkg, &MatchOp::MGE, 2, &MatchOp::MTR, 0, 0));
    }

    #[test]
    fn second_op_also_works() {
        let pkg = &([0, 1, 2, 3].iter().map(|i| Object::Integer(*i as u64).wrap()).collect());
        assert_eq!(2, do_match(pkg, &MatchOp::MTR, 0, &MatchOp::MEQ, 2, 0));
    }

    #[test]
    fn impossible_match_returns_ones() {
        let pkg = &([0, 1, 2, 3].iter().map(|i| Object::Integer(*i as u64).wrap()).collect());
        assert_eq!(ONES, do_match(pkg, &MatchOp::MEQ, 2, &MatchOp::MLT, 1, 0));
    }

    #[test]
    fn offset_works_correctly() {
        let pkg = &([1, 1, 1, 1].iter().map(|i| Object::Integer(*i as u64).wrap()).collect());
        assert_eq!(2, do_match(pkg, &MatchOp::MEQ, 1, &MatchOp::MTR, 0, 2));
    }
}
