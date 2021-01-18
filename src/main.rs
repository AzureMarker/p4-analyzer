#[macro_use]
extern crate lalrpop_util;

use crate::ast::Program;
use crate::convert::ToGcl;
use crate::gcl::GclPredicate;
use lalrpop_util::ParseError;
use std::io::Read;
use z3::{Config, Context, Solver};

mod ast;
mod convert;
mod gcl;
mod to_wlp;
mod verify_wlp;

lalrpop_mod!(
    #[allow(clippy::all)]
    p4_parser
);

fn main() {
    // Read P4 program
    let mut p4_program_str = String::new();
    std::io::stdin()
        .read_to_string(&mut p4_program_str)
        .unwrap();

    // Parse P4
    let p4_program = parse(&p4_program_str);

    // Convert to GCL
    let gcl_programs = p4_program.to_gcl();

    // Check program
    let config = Config::new();
    let context = Context::new(&config);
    let solver = Solver::new(&context);
    for (name, gcl_program) in gcl_programs {
        let wlp_predicate = gcl_program.to_wlp(GclPredicate::Bool(true));
        let z3_bool = wlp_predicate.as_z3_bool(&context);
        solver.assert(&z3_bool);
        let z3_output = solver.check();

        println!(
            "\nProgram '{}'\n  GCL: {}\n  WLP: {}\n  Z3: {}\n  Output: {:?}",
            name, gcl_program, wlp_predicate, z3_bool, z3_output
        );

        solver.reset();
    }
}

/// Parse the P4 program. If there are errors during parsing, the program will
/// exit.
fn parse(p4_program_str: &str) -> Program {
    match p4_parser::ProgramParser::new().parse(p4_program_str) {
        Ok(parsed_ast) => {
            println!("{:#?}\n", parsed_ast);
            parsed_ast
        }
        Err(ParseError::InvalidToken { location }) => {
            let (line, col) = index_to_line_col(p4_program_str, location);
            eprintln!("Invalid token at line {}, column {}", line, col);
            std::process::exit(1);
        }
        Err(ParseError::UnrecognizedToken {
            token: (lspan, token, _rspan),
            expected,
        }) => {
            let (line, col) = index_to_line_col(p4_program_str, lspan);
            eprintln!(
                "Unrecognized token '{}' at line {}, column {}, expected [{}]",
                token,
                line,
                col,
                expected.join(", ")
            );
            std::process::exit(1);
        }
        Err(ParseError::UnrecognizedEOF { location, expected }) => {
            let (line, col) = index_to_line_col(p4_program_str, location);
            eprintln!(
                "Unexpected EOF at line {}, column {}, expected [{}]",
                line,
                col,
                expected.join(", ")
            );
            std::process::exit(1);
        }
        Err(ParseError::ExtraToken {
            token: (lspan, token, _rspan),
        }) => {
            let (line, col) = index_to_line_col(p4_program_str, lspan);
            eprintln!(
                "Unexpected extra token '{}' at line {}, column {}",
                token, line, col
            );
            std::process::exit(1);
        }
        Err(ParseError::User { error }) => {
            eprintln!("{}", error);
            std::process::exit(1);
        }
    }
}

/// Convert an index of the file into a line and column index
fn index_to_line_col(file_str: &str, index: usize) -> (usize, usize) {
    let line = file_str
        .chars()
        .enumerate()
        .take_while(|(i, _)| *i != index)
        .filter(|(_, c)| *c == '\n')
        .count()
        + 1;
    let column = file_str[0..index]
        .chars()
        .rev()
        .take_while(|c| *c != '\n')
        .count()
        + 1;

    (line, column)
}
