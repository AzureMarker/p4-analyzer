#[macro_use]
extern crate lalrpop_util;

use crate::ast::Program;
use crate::convert::ToGcl;
use crate::gcl::GclPredicate;
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
    let p4_program: Program = p4_parser::ProgramParser::new()
        .parse(&p4_program_str)
        .unwrap();

    println!("{:#?}\n", p4_program);

    let gcl_programs = p4_program.to_gcl();
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
