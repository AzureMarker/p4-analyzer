#[macro_use]
extern crate lalrpop_util;

use crate::ast::Program;
use crate::convert::ToGcl;
use crate::gcl::{GclGraph, GclPredicate};
use crate::lexer::{LalrpopLexerIter, Token};
use lalrpop_util::ParseError;
use logos::Logos;
use petgraph::dot::Dot;
use petgraph::graph::NodeIndex;
use petgraph::visit::IntoNodeReferences;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Read;
use std::ops::Deref;
use std::time::Instant;
use z3::{Config, Context, SatResult, Solver};

mod ast;
mod convert;
mod gcl;
mod lexer;
mod to_wlp;
mod verify_wlp;

lalrpop_mod!(
    #[allow(clippy::all)]
    p4_parser
);

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let args: Vec<&str> = args.iter().map(String::as_str).collect::<Vec<_>>();

    // Only check reachability of bug nodes by default
    let mut only_bugs = true;

    match args.as_slice() {
        [_, "--full-reachability"] => only_bugs = false,
        [_] | [] => {}
        [name, ..] => {
            eprintln!("Usage: {} [--full-reachability]", name);
            return;
        }
    }

    // Read P4 program
    let mut p4_program_str = String::new();
    std::io::stdin()
        .read_to_string(&mut p4_program_str)
        .unwrap();

    // Parse P4
    let parse_start = Instant::now();
    let p4_program = parse(&p4_program_str);
    let time_to_parse = parse_start.elapsed();

    // Convert to GCL
    let gcl_start = Instant::now();
    let mut graph = GclGraph::new();
    let gcl_start_node = p4_program.to_gcl(&mut graph);
    let time_to_gcl = gcl_start.elapsed();

    // Calculate the weakest liberal precondition for each node
    let wlp_start = Instant::now();
    let node_wlp = graph.to_wlp();
    let time_to_wlp = wlp_start.elapsed();
    display_wlp(&graph, &node_wlp);

    // Calculate reachability
    let reachable_start = Instant::now();
    let is_reachable = calculate_reachable(&graph, &node_wlp, only_bugs);
    let time_to_reachable = reachable_start.elapsed();

    // Print out the graphviz representation
    let graphviz = make_graphviz(&graph, &is_reachable);
    println!("\n{}", graphviz);

    // Show all reachable bugs
    display_bugs(&graph, &is_reachable, gcl_start_node);

    println!(
        "\nTime to parse P4: {}ms\n\
         Time to convert to GCL: {}ms\n\
         Time to calculate WLP: {}ms\n\
         Time to calculate reachability: {}ms\n\
         Total time: {}ms",
        time_to_parse.as_millis(),
        time_to_gcl.as_millis(),
        time_to_wlp.as_millis(),
        time_to_reachable.as_millis(),
        parse_start.elapsed().as_millis()
    );
}

fn display_wlp(graph: &GclGraph, node_wlp: &HashMap<NodeIndex, GclPredicate>) {
    println!("Weakest Liberal Preconditions:");
    for (node_idx, wlp) in node_wlp {
        let node_name = &graph.node_weight(*node_idx).unwrap().name;

        println!("Node '{}': {}", node_name, wlp);
    }
}

fn calculate_reachable(
    graph: &GclGraph,
    node_wlp: &HashMap<NodeIndex, GclPredicate>,
    only_bugs: bool,
) -> HashMap<NodeIndex, bool> {
    // TODO: idea: go in reverse topological sort, propagate reachability
    //       when the node is reachable and has a single parent (in-edge).
    //       This would avoid unnecessary Z3 calls, but only in
    //       full-reachability mode. This also may not be necessary if CFG
    //       optimizations merge such nodes together.

    let config = Config::new();
    let context = Context::new(&config);
    let solver = Solver::new(&context);

    graph
        .node_references()
        .filter_map(|(node_idx, node)| {
            if only_bugs && !node.is_bug() {
                return None;
            }

            let wlp = node_wlp.get(&node_idx).unwrap();
            solver.assert(&wlp.as_z3_ast(&context).as_bool().unwrap());
            let is_reachable = solver.check() == SatResult::Sat;
            solver.reset();

            Some((node_idx, is_reachable))
        })
        .collect()
}

fn make_graphviz(graph: &GclGraph, is_reachable: &HashMap<NodeIndex, bool>) -> String {
    let get_node_attributes = |_graph, (node_idx, _)| match is_reachable.get(&node_idx) {
        Some(true) => "shape = box, color = green".to_string(),
        Some(false) => "shape = box, color = red".to_string(),
        None => "shape = box, color = grey".to_string(),
    };
    let graphviz_graph = Dot::with_attr_getters(
        graph.deref(),
        &[],
        &|_graph, _edge| String::new(),
        &get_node_attributes,
    );

    graphviz_graph.to_string()
}

fn display_bugs(graph: &GclGraph, is_reachable: &HashMap<NodeIndex, bool>, start_idx: NodeIndex) {
    let mut found_bug = false;

    for (node_idx, node) in graph.node_references() {
        if node.is_bug() && *is_reachable.get(&node_idx).unwrap_or(&false) {
            found_bug = true;
            let path = path_to(graph, start_idx, node_idx).map(|path| {
                // Get the name of each node
                path.into_iter()
                    .map(|node_idx| graph.node_weight(node_idx).unwrap().name.as_str())
                    .collect::<Vec<_>>()
            });
            println!("Found bug: {:?}\nPath = {:?}", node, path);
        }
    }

    if !found_bug {
        println!("No bugs found!");
    }
}

fn path_to(graph: &GclGraph, start_idx: NodeIndex, node_idx: NodeIndex) -> Option<Vec<NodeIndex>> {
    petgraph::algo::all_simple_paths(graph.deref(), start_idx, node_idx, 1, None).next()
}

/// Parse the P4 program. If there are errors during parsing, the program will
/// exit.
fn parse(p4_program_str: &str) -> Program {
    let lexer_state = RefCell::default();
    let lexer = Token::lexer_with_extras(p4_program_str, &lexer_state);
    let lexer_iter = LalrpopLexerIter::new(lexer);

    match p4_parser::ProgramParser::new().parse(p4_program_str, &lexer_state, lexer_iter) {
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
                "Unrecognized token '{:?}' at line {}, column {}, expected [{}]",
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
                "Unexpected extra token '{:?}' at line {}, column {}",
                token, line, col
            );
            std::process::exit(1);
        }
        Err(ParseError::User { error }) => {
            let token = &p4_program_str[error.clone()];
            let (line, col) = index_to_line_col(p4_program_str, error.start);
            eprintln!("Invalid token '{}' at line {}, column {}", token, line, col);
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
