pub(crate) macro check_err($parse: expr, $error: pat, $remains: expr) {
    match $parse {
        Ok(result) => panic!("Expected Err, got {:#?}", result),
        Err((remains, $error)) if *remains == *$remains => (),
        Err((_, err)) => panic!("Got wrong error: {:?}", err),
    }
}

pub(crate) macro check_ok($parse: expr, $expected: expr, $remains: expr) {
    match $parse {
        Ok((remains, result)) if remains == *$remains && result == $expected => (),
        Ok(result) => panic!("Successfully parsed Ok, but it was wrong: {:#?}", result),
        Err((_, err)) => panic!("Expected Ok, got {:#?}", err),
    }
}
