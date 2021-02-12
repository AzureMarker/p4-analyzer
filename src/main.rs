#[macro_use]
extern crate lalrpop_util;

use crate::ast::Program;
use crate::convert::ToGcl;
use crate::gcl::{GclCommand, GclGraph, GclPredicate};
use lalrpop_util::ParseError;
use petgraph::dot::Dot;
use petgraph::graph::NodeIndex;
use std::collections::HashMap;
use std::io::Read;
use std::ops::Deref;
use z3::{Config, Context, SatResult, Solver};

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
    let mut graph = GclGraph::new();
    let gcl_start_node = p4_program.to_gcl(&mut graph);

    // Calculate the weakest liberal precondition for each node
    let node_wlp = graph.to_wlp();
    display_wlp(&graph, &node_wlp);

    // Calculate reachability
    let is_reachable = calculate_reachable(&graph, &node_wlp);

    // Print out the graphviz representation
    let graphviz = make_graphviz(&graph, &is_reachable);
    println!("\n{}", graphviz);

    // Show all reachable bugs
    display_bugs(&graph, &is_reachable, gcl_start_node);
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
) -> HashMap<NodeIndex, bool> {
    let config = Config::new();
    let context = Context::new(&config);
    let solver = Solver::new(&context);

    graph
        .node_indices()
        .map(|node_idx| {
            let wlp = node_wlp.get(&node_idx).unwrap();
            solver.assert(&wlp.as_z3_bool(&context));
            let is_reachable = solver.check() == SatResult::Sat;
            solver.reset();

            (node_idx, is_reachable)
        })
        .collect()
}

fn make_graphviz(graph: &GclGraph, is_reachable: &HashMap<NodeIndex, bool>) -> String {
    let get_node_attributes = |_graph, (node_idx, _)| {
        if *is_reachable.get(&node_idx).unwrap() {
            "shape = box, color = green".to_string()
        } else {
            "shape = box, color = red".to_string()
        }
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
    for node_idx in graph.node_indices() {
        let node = graph.node_weight(node_idx).unwrap();

        if matches!(node.command, GclCommand::Bug) && *is_reachable.get(&node_idx).unwrap() {
            let path = path_to(graph, start_idx, node_idx).map(|path| {
                // Get the name of each node
                path.into_iter()
                    .map(|node_idx| graph.node_weight(node_idx).unwrap().name.as_str())
                    .collect::<Vec<_>>()
            });
            println!("Found bug: {:?}\nPath = {:?}", node, path);
        }
    }
}

fn path_to(graph: &GclGraph, start_idx: NodeIndex, node_idx: NodeIndex) -> Option<Vec<NodeIndex>> {
    petgraph::algo::all_simple_paths(graph.deref(), start_idx, node_idx, 1, None).next()
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
