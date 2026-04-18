use acpi::aml::object::Object;

#[derive(Clone, Debug)]
pub enum ExpectedResult {
    Integer(u64),
    String(String),
}

pub fn result_matches(expected: &ExpectedResult, actual: &Object) -> bool {
    match (expected, actual) {
        (ExpectedResult::Integer(expected), Object::Integer(actual)) => expected == actual,
        (ExpectedResult::String(expected), Object::String(actual)) => expected == actual,
        _ => false,
    }
}
