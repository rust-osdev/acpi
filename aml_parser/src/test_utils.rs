pub(crate) macro check_err($parse: expr, $error: pat, $remains: expr) {
    match $parse {
        Ok(result) => panic!("Expected Err, got {:#?}", result),
        Err((remains, _, $error)) if *remains == *$remains => (),
        Err((remains, _, $error)) => panic!("Correct error, incorrect stream returned: {:x?}", remains),
        Err((_, _, err)) => panic!("Got wrong error: {:?}", err),
    }
}

pub(crate) macro check_ok($parse: expr, $expected: expr, $remains: expr) {
    match $parse {
        Ok((remains, _, ref result)) if remains == *$remains && result == &$expected => (),
        Ok((remains, _, ref result)) if result == &$expected => {
            panic!("Correct result, incorrect slice returned: {:x?}", remains)
        }
        Ok(result) => panic!("Successfully parsed Ok, but it was wrong: {:#?}", result),
        Err((_, _, err)) => panic!("Expected Ok, got {:#?}", err),
    }
}
