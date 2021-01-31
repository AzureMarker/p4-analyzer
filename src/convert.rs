//! Convert P4 to GCL

use crate::ast::{
    ActionDecl, Assignment, BlockStatement, ConstantDecl, ControlDecl, ControlLocalDecl,
    Declaration, Expr, IfStatement, Instantiation, Program, Statement, StatementOrDecl,
    VariableDecl,
};
use crate::gcl::{
    Flatten, GclAssignment, GclCommand, GclGraph, GclNode, GclNodeRange, GclPredicate,
};
use either::Either;
use petgraph::graph::NodeIndex;

/// Trait for converting a P4 AST node into GCL
pub trait ToGcl {
    type Output;

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output;
}

impl ToGcl for Program {
    /// The starting node
    type Output = NodeIndex;

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        let mut commands = Vec::new();

        for decl in &self.declarations {
            match decl {
                Declaration::Constant(const_decl) => commands.push(const_decl.to_gcl(graph)),
                // The nodes are added to the graph automatically
                Declaration::Control(control) => {
                    control.to_gcl(graph);
                }
                Declaration::Instantiation(instantiation) => {
                    commands.push(instantiation.to_gcl(graph))
                }
            }
        }

        graph.add_node(GclNode {
            pre_condition: GclPredicate::default(), // todo
            name: "start".to_string(),
            command: commands.flatten(),
        })

        // TODO: Parse the main decl and create driver GCL

        // self.declarations
        //     .iter()
        //     .flat_map(Declaration::to_gcl)
        //     .collect()
    }
}

// impl ToGcl for Declaration {
//     type Output = Either<GclCommand, (String, String)>;
//
//     fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
//         match self {
//             Declaration::Control(control) => Either::Right(control.to_gcl(graph)),
//             Declaration::Constant(const_decl) => Either::Left(const_decl.to_gcl(graph)),
//         }
//     }
// }

impl ToGcl for ControlDecl {
    type Output = GclNodeRange;

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        let mut commands = Vec::new();

        // Collect all of the top level local declarations (e.g. actions) and
        // local declarations (e.g. variables).
        for local_decl in &self.local_decls {
            match local_decl {
                ControlLocalDecl::Variable(var_decl) => {
                    commands.push(StatementOrDecl::VariableDecl(var_decl.clone()));
                }
                ControlLocalDecl::Instantiation(instantiation) => {
                    commands.push(StatementOrDecl::Instantiation(instantiation.clone()))
                }
                ControlLocalDecl::Constant(const_decl) => {
                    commands.push(StatementOrDecl::ConstantDecl(const_decl.clone()))
                }
                ControlLocalDecl::Action(action_decl) => {
                    let action_range = action_decl.to_gcl(graph);

                    // Register the action under the namespace of this control block
                    graph.register_function(
                        format!("{}::{}", self.name, action_decl.name),
                        action_range,
                    );
                }
            }
        }

        // Add in statements from the apply block
        commands.extend_from_slice(&self.apply_body.0);

        // Create the block node
        let block_range = BlockStatement(commands).to_gcl(graph);

        // Create the control node
        let node_idx = graph.add_node(GclNode {
            pre_condition: GclPredicate::default(), // todo
            name: format!("__control__{}", self.name),
            command: GclCommand::default(),
        });
        graph.add_edge(node_idx, block_range.start, GclPredicate::default());

        GclNodeRange {
            start: node_idx,
            end: block_range.end,
        }
    }
}

impl ToGcl for ConstantDecl {
    type Output = GclCommand;

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        GclCommand::Sequence(
            Box::new(GclCommand::Assignment(GclAssignment {
                name: format!("_has_value__{}", self.name),
                pred: GclPredicate::Bool(true),
            })),
            Box::new(GclCommand::Assignment(GclAssignment {
                name: self.name.clone(),
                pred: self.value.to_gcl(graph),
            })),
        )
    }
}

impl ToGcl for ActionDecl {
    type Output = GclNodeRange;

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        let body_range = self.body.to_gcl(graph);
        let start_node_idx = graph.add_node(GclNode {
            pre_condition: GclPredicate::default(),
            name: format!("__action__{}", self.name),
            command: GclCommand::default(),
        });
        graph.add_edge(start_node_idx, body_range.start, GclPredicate::default());

        // Note: the action is registered as a function with the graph in
        // ControlDecl::to_gcl so it can be namespaced under the control block.

        GclNodeRange {
            start: start_node_idx,
            end: body_range.end,
        }
    }
}

impl ToGcl for BlockStatement {
    type Output = GclNodeRange;

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        let mut current_commands: Vec<GclCommand> = Vec::new();
        let mut block_start_end: Option<GclNodeRange> = None;

        fn create_node_from_commands(
            current_commands: &mut Vec<GclCommand>,
            graph: &mut GclGraph,
            block_start_end: &mut Option<GclNodeRange>,
        ) {
            // Make a node from the commands
            let name = graph.create_name("block_stmt_body");
            let node_idx = graph.add_node(GclNode {
                pre_condition: GclPredicate::default(), // todo
                name,
                command: std::mem::take(current_commands).flatten(),
            });

            // Hook up the node to the end of the node chain
            if let Some(range) = block_start_end {
                graph.add_edge(range.end, node_idx, GclPredicate::default());
                *block_start_end = Some(GclNodeRange {
                    start: range.start,
                    end: node_idx,
                });
            } else {
                *block_start_end = Some(GclNodeRange {
                    start: node_idx,
                    end: node_idx,
                });
            }
        };

        // Expand each statement and collect all of the nodes (e.g. from if
        // statements) and simple commands (e.g. variable declarations).
        for statement_or_decl in &self.0 {
            match statement_or_decl.to_gcl(graph) {
                Either::Left(command) => {
                    current_commands.push(command);
                }
                Either::Right(GclNodeRange {
                    start: start_node,
                    end: end_node,
                }) => {
                    if !current_commands.is_empty() {
                        create_node_from_commands(
                            &mut current_commands,
                            graph,
                            &mut block_start_end,
                        );
                    }

                    if let Some(range) = block_start_end {
                        graph.add_edge(range.end, start_node, GclPredicate::default());
                        block_start_end = Some(GclNodeRange {
                            start: range.start,
                            end: end_node,
                        });
                    } else {
                        block_start_end = Some(GclNodeRange {
                            start: start_node,
                            end: end_node,
                        });
                    }
                }
            }
        }

        // Make sure to add any trailing commands to the graph, and ensure that
        // we have at least one node in the range.
        if !current_commands.is_empty() || block_start_end.is_none() {
            create_node_from_commands(&mut current_commands, graph, &mut block_start_end);
        }

        block_start_end.unwrap()
    }
}

impl ToGcl for StatementOrDecl {
    type Output = Either<GclCommand, GclNodeRange>;

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        match self {
            StatementOrDecl::Statement(statement) => Either::Right(statement.to_gcl(graph)),
            StatementOrDecl::VariableDecl(var_decl) => Either::Left(var_decl.to_gcl(graph)),
            StatementOrDecl::ConstantDecl(const_decl) => Either::Left(const_decl.to_gcl(graph)),
            StatementOrDecl::Instantiation(instantiation) => {
                Either::Left(instantiation.to_gcl(graph))
            }
        }
    }
}

impl ToGcl for Statement {
    /// A statement expands to a set of one or more nodes
    type Output = GclNodeRange;

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        match self {
            Statement::Block(block) => block.to_gcl(graph),
            Statement::If(if_statement) => if_statement.to_gcl(graph),
            Statement::Assignment(assignment) => {
                let node_name = assignment.to_gcl(graph);
                GclNodeRange {
                    start: node_name,
                    end: node_name,
                }
            }
        }
    }
}

impl ToGcl for VariableDecl {
    type Output = GclCommand;

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        let has_decl_command = GclCommand::Assignment(GclAssignment {
            name: format!("_declared_var__{}", self.name),
            pred: GclPredicate::Bool(true),
        });
        let has_value_command = GclCommand::Assignment(GclAssignment {
            name: format!("_has_value__{}", self.name),
            pred: GclPredicate::Bool(self.value.is_some()),
        });
        let ghost_variables =
            GclCommand::Sequence(Box::new(has_decl_command), Box::new(has_value_command));

        match self.value.as_ref() {
            Some(value) => GclCommand::Sequence(
                Box::new(ghost_variables),
                Box::new(GclCommand::Assignment(GclAssignment {
                    name: self.name.clone(),
                    pred: value.to_gcl(graph),
                })),
            ),
            None => ghost_variables,
        }
    }
}

impl ToGcl for Instantiation {
    type Output = GclCommand;

    fn to_gcl(&self, _graph: &mut GclGraph) -> Self::Output {
        GclCommand::Assignment(GclAssignment {
            name: format!("_has_value__{}", self.name),
            pred: GclPredicate::Bool(true),
        })
    }
}

impl ToGcl for Assignment {
    /// The GCL node
    type Output = NodeIndex;

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        let node = GclNode {
            pre_condition: GclPredicate::Var(format!("_declared_var__{}", self.name)),
            name: graph.create_name(&format!("__assignment__{}", self.name)),
            command: GclCommand::Sequence(
                Box::new(GclCommand::Assignment(GclAssignment {
                    name: format!("_has_value__{}", self.name),
                    pred: GclPredicate::Bool(true),
                })),
                Box::new(GclCommand::Assignment(GclAssignment {
                    name: self.name.clone(),
                    pred: self.value.to_gcl(graph),
                })),
            ),
        };

        graph.add_node(node)
    }
}

impl ToGcl for IfStatement {
    type Output = GclNodeRange;

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        // Make sure that all the variables read by this conditional have a value
        let vars = self.condition.find_all_vars();
        let var_predicates: Vec<_> = vars
            .into_iter()
            .map(|var| GclPredicate::Var(format!("_has_value__{}", var)))
            .collect();
        let var_value_check_gcl = var_predicates.flatten();

        // Create the end node first so we can refer to its index
        let end_node = GclNode {
            pre_condition: GclPredicate::default(),
            name: graph.create_name("if_end"),
            command: GclCommand::default(),
        };
        let end_node_idx = graph.add_node(end_node);

        // Calculate the if condition predicates
        let pred = self.condition.to_gcl(graph);
        let negated_pred = GclPredicate::Negation(Box::new(pred.clone()));

        // Convert the then and else branches to GCL
        let GclNodeRange {
            start: then_node_start,
            end: then_node_end,
        } = self.then_case.to_gcl(graph);
        let else_node_range = self.else_case.as_ref().map(|stmt| stmt.to_gcl(graph));
        let else_node_idx = if let Some(else_range) = else_node_range {
            else_range.start
        } else {
            end_node_idx
        };

        // Create the jump (start) node
        let jump_node = GclNode {
            pre_condition: var_value_check_gcl,
            name: graph.create_name("if_jump"),
            command: GclCommand::default(),
        };
        let jump_node_idx = graph.add_node(jump_node);
        graph.add_edge(jump_node_idx, then_node_start, pred);
        graph.add_edge(jump_node_idx, else_node_idx, negated_pred);

        // Add edges to the end node from then and else branches
        graph.add_edge(then_node_end, end_node_idx, GclPredicate::default());
        if let Some(else_range) = else_node_range {
            graph.add_edge(else_range.end, end_node_idx, GclPredicate::default());
        }

        GclNodeRange {
            start: jump_node_idx,
            end: end_node_idx,
        }
    }
}

impl ToGcl for Expr {
    type Output = GclPredicate;

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        match self {
            Expr::Bool(b) => GclPredicate::Bool(*b),
            Expr::Var(name) => GclPredicate::Var(name.clone()),
            Expr::And(left, right) => GclPredicate::Conjunction(
                Box::new(left.to_gcl(graph)),
                Box::new(right.to_gcl(graph)),
            ),
            Expr::Or(left, right) => GclPredicate::Disjunction(
                Box::new(left.to_gcl(graph)),
                Box::new(right.to_gcl(graph)),
            ),
            Expr::Negation(inner) => GclPredicate::Negation(Box::new(inner.to_gcl(graph))),
        }
    }
}

impl Expr {
    /// Get all of the variables this expression reads from
    fn find_all_vars(&self) -> Vec<&str> {
        match self {
            Expr::Bool(_) => Vec::new(),
            Expr::Var(name) => vec![name],
            Expr::And(left, right) | Expr::Or(left, right) => {
                let mut vars = left.find_all_vars();
                vars.extend(right.find_all_vars());
                vars
            }
            Expr::Negation(inner) => inner.find_all_vars(),
        }
    }
}
