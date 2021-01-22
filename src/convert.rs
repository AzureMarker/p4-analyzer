//! Convert P4 to GCL

use crate::ast::{
    ActionDecl, Assignment, BlockStatement, ConstantDecl, ControlDecl, ControlLocalDecl,
    Declaration, Expr, IfStatement, Instantiation, Program, Statement, StatementOrDecl,
    VariableDecl,
};
use crate::gcl::{Flatten, GclAssignment, GclCommand, GclGraph, GclJump, GclNode, GclPredicate};
use either::Either;

/// Trait for converting a P4 AST node into GCL
pub trait ToGcl {
    type Output;

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output;
}

impl ToGcl for Program {
    type Output = ();

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

        let start_node = GclNode {
            pre_condition: GclPredicate::default(), // todo
            command: commands.flatten(),
        };
        graph.nodes.insert("start".to_string(), start_node);

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
    type Output = (String, String);

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        let mut block_stmt = BlockStatement(Vec::new());

        // Collect all of the top level local declarations (e.g. actions) and
        // local declarations (e.g. variables).
        for local_decl in &self.local_decls {
            match local_decl {
                ControlLocalDecl::Variable(var_decl) => {
                    block_stmt
                        .0
                        .push(StatementOrDecl::VariableDecl(var_decl.clone()));
                }
                ControlLocalDecl::Instantiation(instantiation) => block_stmt
                    .0
                    .push(StatementOrDecl::Instantiation(instantiation.clone())),
                ControlLocalDecl::Constant(const_decl) => block_stmt
                    .0
                    .push(StatementOrDecl::ConstantDecl(const_decl.clone())),
                ControlLocalDecl::Action(action_decl) => {
                    // The nodes will be placed into the graph
                    action_decl.to_gcl(graph);
                }
            }
        }

        // Add in statements from the apply block
        block_stmt.0.extend_from_slice(&self.apply_body.0);

        // Create the block node
        let (block_start, block_end) = block_stmt.to_gcl(graph);

        // Create the control node
        let node_name = format!("__control__{}", self.name);
        let node = GclNode {
            pre_condition: GclPredicate::default(), // todo
            command: GclCommand::Jump(GclJump::Direct {
                next_node: block_start,
            }),
        };

        (node_name, block_end)
    }
}

impl ToGcl for ConstantDecl {
    type Output = GclCommand;

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        GclCommand::Sequence(
            Box::new(GclCommand::Assignment(GclAssignment {
                name: format!("_var_has_value__{}", self.name),
                pred: GclPredicate::Bool(true),
            })),
            Box::new(GclCommand::Assignment(GclAssignment {
                name: self.name.clone(),
                pred: self.value.to_gcl(graph),
            })),
        )
    }
}

impl<T: ToGcl<Output = GclCommand>> ToGcl for [T] {
    type Output = GclCommand;

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        match self.split_first() {
            Some((head, [])) => head.to_gcl(graph),
            Some((head, tail)) => {
                GclCommand::Sequence(Box::new(head.to_gcl(graph)), Box::new(tail.to_gcl(graph)))
            }
            None => GclCommand::default(),
        }
    }
}

impl ToGcl for ControlLocalDecl {
    type Output = Either<GclCommand, (String, String)>;

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        match self {
            ControlLocalDecl::Variable(var_decl) => Either::Left(var_decl.to_gcl(graph)),
            ControlLocalDecl::Instantiation(instantiation) => {
                Either::Left(instantiation.to_gcl(graph))
            }
            ControlLocalDecl::Constant(const_decl) => Either::Left(const_decl.to_gcl(graph)),
            ControlLocalDecl::Action(action_decl) => Either::Right(action_decl.to_gcl(graph)),
        }
    }
}

impl ToGcl for ActionDecl {
    type Output = (String, String);

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        let (body_start, body_end) = self.body.to_gcl(graph);
        let start_name = format!("__action__{}", self.name);
        let end_name = format!("__action_end__{}", self.name);
        graph.set_node_jump(&body_end, end_name.clone());

        graph.nodes.insert(
            start_name.clone(),
            GclNode {
                pre_condition: GclPredicate::default(),
                command: GclCommand::Jump(GclJump::Direct {
                    next_node: body_start,
                }),
            },
        );
        graph.nodes.insert(
            end_name.clone(),
            GclNode {
                pre_condition: GclPredicate::default(),
                command: GclCommand::Jump(GclJump::Pop),
            },
        );

        (start_name, end_name)
    }
}

impl ToGcl for BlockStatement {
    type Output = (String, String);

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        let mut current_commands: Vec<GclCommand> = Vec::new();
        let mut block_start_end: Option<(String, String)> = None;

        fn create_node_from_commands(
            current_commands: &mut Vec<GclCommand>,
            graph: &mut GclGraph,
            block_start_end: &mut Option<(String, String)>,
        ) {
            let command = std::mem::take(current_commands).flatten();
            let node_name = graph.create_name("block_stmt_body");

            if let Some((_block_start, block_end)) = block_start_end {
                graph.set_node_jump(block_end, node_name.clone());
            } else {
                *block_start_end = Some((node_name.clone(), node_name.clone()));
            }

            graph.nodes.insert(
                node_name,
                GclNode {
                    pre_condition: GclPredicate::default(), // todo
                    command,
                },
            );
        };

        // Expand each statement and collect all of the nodes (e.g. from if
        // statements) and simple commands (e.g. variable declarations).
        for statement_or_decl in &self.0 {
            match statement_or_decl.to_gcl(graph) {
                Either::Left(command) => {
                    current_commands.push(command);
                }
                Either::Right((start_node, end_node)) => {
                    if !current_commands.is_empty() {
                        create_node_from_commands(
                            &mut current_commands,
                            graph,
                            &mut block_start_end,
                        );
                    }

                    if let Some((_block_start, block_end)) = &block_start_end {
                        graph.set_node_jump(block_end, start_node);
                    } else {
                        block_start_end = Some((start_node, end_node));
                    }
                }
            }
        }

        if !current_commands.is_empty() {
            create_node_from_commands(&mut current_commands, graph, &mut block_start_end);
        }

        block_start_end.unwrap_or_else(|| {
            // Empty block
            let node_name = graph.create_name("block_stmt_body");
            graph.nodes.insert(
                node_name.clone(),
                GclNode {
                    pre_condition: GclPredicate::default(),
                    command: GclCommand::default(),
                },
            );

            (node_name.clone(), node_name)
        })
    }
}

impl ToGcl for StatementOrDecl {
    type Output = Either<GclCommand, (String, String)>;

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        match self {
            StatementOrDecl::Statement(statement) => statement.to_gcl(graph),
            StatementOrDecl::VariableDecl(var_decl) => Either::Left(var_decl.to_gcl(graph)),
            StatementOrDecl::ConstantDecl(const_decl) => Either::Left(const_decl.to_gcl(graph)),
            StatementOrDecl::Instantiation(instantiation) => {
                Either::Left(instantiation.to_gcl(graph))
            }
        }
    }
}

impl ToGcl for Statement {
    /// A statement either expands to a straightforward command or a set of
    /// nodes. If it's a node, the start and end node names are returned (may be
    /// equal if only one node).
    type Output = Either<GclCommand, (String, String)>;

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        match self {
            Statement::Block(block) => Either::Right(block.to_gcl(graph)),
            Statement::If(if_statement) => Either::Right(if_statement.to_gcl(graph)),
            Statement::Assignment(assignment) => Either::Left(assignment.to_gcl(graph)),
        }
    }
}

impl ToGcl for VariableDecl {
    type Output = GclCommand;

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        let has_value_command = GclCommand::Assignment(GclAssignment {
            name: format!("_var_has_value__{}", self.name),
            pred: GclPredicate::Bool(self.value.is_some()),
        });

        match self.value.as_ref() {
            Some(value) => GclCommand::Sequence(
                Box::new(has_value_command),
                Box::new(GclCommand::Assignment(GclAssignment {
                    name: self.name.clone(),
                    pred: value.to_gcl(graph),
                })),
            ),
            None => has_value_command,
        }
    }
}

impl ToGcl for Instantiation {
    type Output = GclCommand;

    fn to_gcl(&self, _graph: &mut GclGraph) -> Self::Output {
        GclCommand::Assignment(GclAssignment {
            name: format!("_var_has_value__{}", self.name),
            pred: GclPredicate::Bool(true),
        })
    }
}

impl ToGcl for Assignment {
    type Output = GclCommand;

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        GclCommand::Sequence(
            Box::new(GclCommand::Assignment(GclAssignment {
                name: format!("_var_has_value__{}", self.name),
                pred: GclPredicate::Bool(true),
            })),
            Box::new(GclCommand::Assignment(GclAssignment {
                name: self.name.clone(),
                pred: self.value.to_gcl(graph),
            })),
        )
    }
}

impl ToGcl for IfStatement {
    /// The start and end node names
    type Output = (String, String);

    fn to_gcl(&self, graph: &mut GclGraph) -> Self::Output {
        // Make sure that all the variables read by this conditional have a value
        let vars = self.condition.find_all_vars();
        let var_predicates: Vec<_> = vars
            .into_iter()
            .map(|var| GclPredicate::Var(format!("_var_has_value__{}", var)))
            .collect();
        let var_value_check_gcl = var_predicates.flatten();

        let jump_node_name = graph.create_name("if_jump");
        let end_node_name = graph.create_name("if_end");

        let pred = self.condition.to_gcl(graph);
        let negated_pred = GclPredicate::Negation(Box::new(pred.clone()));
        let (then_node_start, then_node_end) = self.then_case.to_gcl(graph);
        let else_node_names = self.else_case.as_ref().map(|stmt| stmt.to_gcl(graph));

        let jump_node = GclNode {
            pre_condition: var_value_check_gcl,
            command: GclCommand::Choice(
                Box::new(GclCommand::Sequence(
                    Box::new(GclCommand::Assumption(pred)),
                    Box::new(GclCommand::Jump(GclJump::Direct {
                        next_node: then_node_start,
                    })),
                )),
                Box::new(GclCommand::Sequence(
                    Box::new(GclCommand::Assumption(negated_pred)),
                    Box::new(GclCommand::Jump(GclJump::Direct {
                        next_node: if let Some((else_node_start, _)) = &else_node_names {
                            else_node_start.clone()
                        } else {
                            end_node_name.clone()
                        },
                    })),
                )),
            ),
        };

        graph.set_node_jump(&then_node_end, end_node_name.clone());
        if let Some((_, else_node_end)) = &else_node_names {
            graph.set_node_jump(&else_node_end, end_node_name.clone());
        }

        // let then_node = GclNode {
        //     pre_condition: GclPredicate::default(), // todo
        //     post_condition: GclPredicate::default(),
        //     command: GclCommand::Sequence(
        //         Box::new(then_case_gcl),
        //         Box::new(GclCommand::Jump(GclJump::Direct {
        //             next_node: end_node_name.clone(),
        //         })),
        //     ),
        // };
        // let else_node = GclNode {
        //     pre_condition: GclPredicate::default(), // todo
        //     command: GclCommand::Sequence(
        //         Box::new(else_case_gcl),
        //         Box::new(GclCommand::Jump(GclJump {
        //             kind: GclJumpKind::Direct {
        //                 next_node: end_node_name.clone(),
        //             },
        //             post_condition: GclPredicate::default(), // todo
        //         })),
        //     ),
        // };
        let end_node = GclNode {
            pre_condition: GclPredicate::default(),
            command: GclCommand::default(), // TODO: this should link up later to the next node
        };

        graph.nodes.insert(jump_node_name.clone(), jump_node);
        // graph.nodes.insert(then_node_name, then_node);
        // graph.nodes.insert(else_node_name, else_node);
        graph.nodes.insert(end_node_name.clone(), end_node);

        (jump_node_name, end_node_name)

        // // A choice of two branches.
        // // The "then" branch assumes the conditional, while the "else"
        // // branch assumes the negated conditional.
        // // TODO: is this the correct translation of an "if" into p4v's
        // //       version of GCL?
        // GclCommand::Sequence(
        //     Box::new(var_value_check_gcl),
        //     Box::new(GclCommand::Choice(
        //         Box::new(GclCommand::Sequence(
        //             Box::new(GclCommand::Assumption(pred)),
        //             Box::new(then_case_gcl),
        //         )),
        //         Box::new(GclCommand::Sequence(
        //             Box::new(GclCommand::Assumption(negated_pred)),
        //             Box::new(else_case_gcl),
        //         )),
        //     )),
        // )
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
    fn find_all_vars(&self) -> Vec<String> {
        match self {
            Expr::Bool(_) => Vec::new(),
            Expr::Var(name) => vec![name.clone()],
            Expr::And(left, right) | Expr::Or(left, right) => {
                let mut vars = left.find_all_vars();
                vars.extend(right.find_all_vars());
                vars
            }
            Expr::Negation(inner) => inner.find_all_vars(),
        }
    }
}
