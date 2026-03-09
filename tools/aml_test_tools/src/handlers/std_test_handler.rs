use crate::handlers::{
    check_cmd_handler::{AcpiCommands as Check, CheckCommandHandler},
    listed_response_handler::{AcpiCommands as Response, ListedResponseHandler},
};
use acpi::Handler;

/// Simplifies the construction of a standard test [`Handler`].
///
/// By combining the commands and responses into a single tuple, it hopefully makes test files
/// easier to read and write.
pub type Command = (Check, Response);

/// Construct a "standard handler".
///
/// This is the handler that I expect to be used most often in tests. (A [`CheckCommandHandler`]
/// wrapping a [`ListedResponseHandler`]).
pub fn construct_std_handler(commands: Vec<Command>) -> impl Handler {
    let (c, r): (Vec<Check>, Vec<Response>) = commands.into_iter().unzip();

    CheckCommandHandler::new(c, ListedResponseHandler::new(r))
}
