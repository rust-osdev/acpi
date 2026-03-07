use crate::test_infra::{
    check_cmd_handler::{AcpiCommands as Check, CheckCommandHandler},
    listed_response_handler::{AcpiCommands as Response, ListedResponseHandler},
};
use acpi::Handler;

pub type Command = (Check, Response);

pub fn construct_std_handler(commands: Vec<Command>) -> impl Handler {
    let (c, r): (Vec<Check>, Vec<Response>) = commands.into_iter().unzip();

    CheckCommandHandler::new(c, ListedResponseHandler::new(r))
}
