use crate::aml::{AmlError, object::Package};

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
            return idx as u64;
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
pub enum MatchOp {
    MTR,
    MEQ,
    MLE,
    MLT,
    MGE,
    MGT,
}

impl From<u16> for MatchOp {
    fn from(value: u16) -> Self {
        match value {
            0 => Self::MTR,
            1 => Self::MEQ,
            2 => Self::MLE,
            3 => Self::MLT,
            4 => Self::MGE,
            5 => Self::MGT,
            _ => panic!("Invalid Match Opcode"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::aml::object::Object;

    #[test]
    fn empty_package_doesnt_match() {
        assert_eq!(ONES, do_match(&vec![], &MatchOp::MEQ, 0, &MatchOp::MTR, 0, 0));
    }

    #[test]
    fn basic_match() {
        let pkg = &([0, 1, 2, 3].iter().map(|i| Object::Integer(*i as u64).wrap()).collect());
        assert_eq!(2, do_match(pkg, &MatchOp::MEQ, 2, &MatchOp::MTR, 0, 0));
    }
}
