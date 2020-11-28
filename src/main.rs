#[macro_use]
extern crate lalrpop_util;

use std::io::Read;

mod ast;

lalrpop_mod!(p4_parser);

fn main() {
    // Read P4 program
    let mut p4_program_str = String::new();
    std::io::stdin()
        .read_to_string(&mut p4_program_str)
        .unwrap();

    // Parse P4
    let p4_program = p4_parser::ProgramParser::new()
        .parse(&p4_program_str)
        .unwrap();

    println!("{:#?}", p4_program);
}
