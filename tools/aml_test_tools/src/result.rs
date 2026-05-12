use acpi::aml::object::Object;
use core::alloc::Allocator;

#[derive(Clone, Debug)]
pub enum ExpectedResult {
    Integer(u64),
    String(String),
}

pub fn result_matches<A: Allocator + Clone>(expected: &ExpectedResult, actual: &Object<A>) -> bool {
    match (expected, actual) {
        (ExpectedResult::Integer(expected), Object::Integer(actual)) => expected == actual,
        // Compare the std String against AmlString's str view.
        (ExpectedResult::String(expected), Object::String(actual)) => expected.as_str() == actual.as_str(),
        _ => false,
    }
}
