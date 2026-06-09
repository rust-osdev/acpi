use acpi::Handle;
use aml_test_tools::{
    handlers::std_test_handler::{acquire, construct_std_handler, create_mutex, release},
    new_interpreter,
};

#[test]
fn acquire_release_global_lock_balances_handler_mutex() {
    const GLOBAL_LOCK_MUTEX: Handle = Handle(1);
    const INFINITE_TIMEOUT: u16 = 0xffff;

    let handler = construct_std_handler(vec![
        create_mutex(),
        acquire(GLOBAL_LOCK_MUTEX, INFINITE_TIMEOUT),
        release(GLOBAL_LOCK_MUTEX),
    ]);
    let interpreter = new_interpreter(handler);

    interpreter.acquire_global_lock(INFINITE_TIMEOUT).unwrap();
    interpreter.release_global_lock().unwrap();
}
