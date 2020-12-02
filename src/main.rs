#[macro_use]
extern crate lalrpop_util;

use crate::ast::Program;
use crate::convert::ToGcl;
use std::io::Read;

mod ast;
mod convert;
mod gcl;

lalrpop_mod!(p4_parser);

fn main() {
    // Read P4 program
    let mut p4_program_str = String::new();
    std::io::stdin()
        .read_to_string(&mut p4_program_str)
        .unwrap();

    // Parse P4
    let p4_program: Program = p4_parser::ProgramParser::new()
        .parse(&p4_program_str)
        .unwrap();

    println!("{:#?}\n", p4_program);

    let gcl_programs = p4_program.to_gcl();
    for (i, gcl_program) in gcl_programs.iter().enumerate() {
        println!("Program {}: {}", i + 1, gcl_program);
    }
}
