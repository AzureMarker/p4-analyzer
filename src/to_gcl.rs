//! Convert P4 to GCL

use crate::ast::Direction;
use crate::gcl::{
    GclAssignment, GclBinOp, GclCommand, GclExpr, GclExprData, GclFact, GclGraph, GclLValue,
    GclNode, GclNodeRange, MemoryLocation,
};
use crate::ir::{
    IrActionDecl, IrAssignment, IrBlockStatement, IrControlDecl, IrControlLocalDecl, IrDeclaration,
    IrExpr, IrExprData, IrFunctionCall, IrIfStatement, IrInstantiation, IrLValue, IrLValueData,
    IrProgram, IrStatement, IrStatementOrDecl, IrType, IrVariableDecl,
};
use crate::type_checker::ProgramMetadata;
use either::Either;
use petgraph::graph::NodeIndex;

/// Trait for converting a P4 AST node into GCL
pub trait ToGcl {
    type Output;

    fn to_gcl(&self, graph: &mut GclGraph, metadata: &ProgramMetadata) -> Self::Output;
}

impl ToGcl for IrProgram {
    /// The starting node
    type Output = NodeIndex;

    fn to_gcl(&self, graph: &mut GclGraph, metadata: &ProgramMetadata) -> Self::Output {
        let start_idx = graph.add_node(GclNode {
            name: "start".to_string(),
            commands: Vec::new(),
        });

        let mut commands = Vec::new();
        let mut node_range = GclNodeRange {
            start: start_idx,
            end: start_idx,
        };
        let mut control_idx = None;

        for decl in &self.declarations {
            match decl {
                IrDeclaration::Constant(const_decl) => {
                    // Add onto the node chain
                    let range = const_decl.to_gcl(graph, metadata);
                    graph.add_edge(node_range.end, range.start, GclExpr::default());
                    node_range.end = range.end;
                }
                // The nodes are added to the graph automatically
                IrDeclaration::Control(control) => {
                    control_idx = Some(control.to_gcl(graph, metadata).start);
                }
                IrDeclaration::Instantiation(instantiation) => {
                    commands.push(instantiation.to_gcl(graph, metadata))
                }
            }
        }

        graph.node_weight_mut(start_idx).unwrap().commands = commands;

        // let main_decl = self
        //     .declarations
        //     .last()
        //     .and_then(|decl| match decl {
        //         IrDeclaration::Instantiation(instantiation) /*if instantiation.name == "main"*/ => {
        //             Some(instantiation)
        //         }
        //         _ => None,
        //     })
        //     .expect("Missing main declaration");

        // if !matches!(
        //     &main_decl.ty,
        //     IrType::Struct(IrStructType { id, .. })
        //         if metadata.type_names.get(&id).map(String::as_str) == Some("V1Switch")
        // ) {
        //     panic!(
        //         "Expected type of main to be 'V1Switch', got '{:?}'",
        //         main_decl.ty
        //     );
        // }

        // FIXME: This is a hard-coded way of connecting start to the control
        //        block. Instead, we should parse the main decl and use that info.
        if let Some(idx) = control_idx {
            graph.add_edge(start_idx, idx, GclExpr::default());
        }

        start_idx

        // TODO: Parse the main decl and create driver GCL
    }
}

impl ToGcl for IrControlDecl {
    type Output = GclNodeRange;

    fn to_gcl(&self, graph: &mut GclGraph, metadata: &ProgramMetadata) -> Self::Output {
        let mut commands = Vec::new();
        let mut param_init_commands = Vec::new();
        let mut param_end_commands = Vec::new();

        // Add in each param
        for param in &self.params {
            let loc = graph.get_var_location(param.id);
            param_end_commands.push(GclCommand::RemoveFact(GclFact::HasValue(loc)));

            if let Direction::In | Direction::InOut = param.direction {
                // "in" and "inout" parameters can be read from
                param_init_commands.push(GclCommand::AddFact(GclFact::HasValue(loc)));
            }
        }
        let param_init_node_name = graph.create_name("control_params_init");
        let param_end_node_name = graph.create_name("control_params_end");
        let param_init_node_idx = graph.add_node(GclNode {
            name: param_init_node_name,
            commands: param_init_commands,
        });
        let param_end_node_idx = graph.add_node(GclNode {
            name: param_end_node_name,
            commands: param_end_commands,
        });

        // Collect all of the top level local declarations (e.g. actions) and
        // local declarations (e.g. variables).
        for local_decl in &self.local_decls {
            match local_decl {
                IrControlLocalDecl::Variable(var_decl) => {
                    commands.push(IrStatementOrDecl::VariableDecl(var_decl.clone()));
                }
                IrControlLocalDecl::Instantiation(instantiation) => {
                    commands.push(IrStatementOrDecl::Instantiation(instantiation.clone()))
                }
                IrControlLocalDecl::Action(action_decl) => {
                    let action_range = action_decl.to_gcl(graph, metadata);

                    // Register the action under the namespace of this control block
                    graph.register_function(
                        // FIXME: Check if we actually need namespacing
                        // format!("{}::{}", self.name, action_decl.name),
                        action_decl.id,
                        action_range,
                    );
                }
                IrControlLocalDecl::Table(_table_decl) => {
                    // TODO
                }
            }
        }

        // Add in statements from the apply block
        commands.extend_from_slice(&self.apply_body.0);

        // Create the block node
        let block_range = IrBlockStatement(commands).to_gcl(graph, metadata);
        graph.add_edge(param_init_node_idx, block_range.start, GclExpr::default());
        graph.add_edge(block_range.end, param_end_node_idx, GclExpr::default());

        GclNodeRange {
            start: param_init_node_idx,
            end: param_end_node_idx,
        }
    }
}

impl ToGcl for IrActionDecl {
    type Output = GclNodeRange;

    fn to_gcl(&self, graph: &mut GclGraph, metadata: &ProgramMetadata) -> Self::Output {
        let body_range = self.body.to_gcl(graph, metadata);
        let start_node_idx = graph.add_node(GclNode {
            name: format!("action__{}", self.id),
            // FIXME: remove ret hack
            commands: vec![GclCommand::Assignment(GclAssignment {
                lvalue: GclLValue::Var(MemoryLocation::ReturnVal),
                expr: GclExpr::bool(true),
            })],
        });
        graph.add_edge(start_node_idx, body_range.start, GclExpr::default());

        // Note: the action is registered as a function with the graph in
        // ControlDecl::to_gcl so it can be namespaced under the control block.

        GclNodeRange {
            start: start_node_idx,
            end: body_range.end,
        }
    }
}

impl ToGcl for IrBlockStatement {
    type Output = GclNodeRange;

    fn to_gcl(&self, graph: &mut GclGraph, metadata: &ProgramMetadata) -> Self::Output {
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
                name,
                commands: std::mem::take(current_commands),
            });

            // Hook up the node to the end of the node chain
            if let Some(range) = block_start_end {
                graph.add_edge(range.end, node_idx, GclExpr::default());
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
        }

        // Expand each statement and collect all of the nodes (e.g. from if
        // statements) and simple commands (e.g. variable declarations).
        for statement_or_decl in &self.0 {
            match statement_or_decl.to_gcl(graph, metadata) {
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
                        graph.add_edge(range.end, start_node, GclExpr::default());
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

impl ToGcl for IrStatementOrDecl {
    type Output = Either<GclCommand, GclNodeRange>;

    fn to_gcl(&self, graph: &mut GclGraph, metadata: &ProgramMetadata) -> Self::Output {
        match self {
            IrStatementOrDecl::Statement(statement) => {
                Either::Right(statement.to_gcl(graph, metadata))
            }
            IrStatementOrDecl::VariableDecl(var_decl) => {
                Either::Right(var_decl.to_gcl(graph, metadata))
            }
            IrStatementOrDecl::Instantiation(instantiation) => {
                Either::Left(instantiation.to_gcl(graph, metadata))
            }
        }
    }
}

impl ToGcl for IrStatement {
    type Output = GclNodeRange;

    fn to_gcl(&self, graph: &mut GclGraph, metadata: &ProgramMetadata) -> Self::Output {
        match self {
            IrStatement::Block(block) => block.to_gcl(graph, metadata),
            IrStatement::If(if_statement) => if_statement.to_gcl(graph, metadata),
            IrStatement::Assignment(assignment) => assignment.to_gcl(graph, metadata),
            IrStatement::FunctionCall(func_call) => func_call.to_gcl(graph, metadata),
        }
    }
}

impl ToGcl for IrVariableDecl {
    type Output = GclNodeRange;

    fn to_gcl(&self, graph: &mut GclGraph, metadata: &ProgramMetadata) -> Self::Output {
        let loc = graph.get_var_location(self.id);
        let name = graph.create_name(&format!("var_decl__{}", self.id));

        match self.value.as_ref() {
            Some(value) => {
                let (value_loc, expr_range) = value.to_gcl(graph, metadata);
                let node_idx = graph.add_node(GclNode {
                    name,
                    commands: vec![
                        GclCommand::Assignment(GclAssignment {
                            lvalue: GclLValue::Var(loc),
                            expr: GclExpr::var(value_loc, value.ty.clone()),
                        }),
                        GclCommand::AddFact(GclFact::HasValue(loc)),
                    ],
                });
                graph.add_edge(expr_range.end, node_idx, GclExpr::default());

                GclNodeRange {
                    start: expr_range.start,
                    end: node_idx,
                }
            }
            None => {
                let node_idx = graph.add_node(GclNode {
                    name,
                    commands: Vec::new(),
                });

                GclNodeRange {
                    start: node_idx,
                    end: node_idx,
                }
            }
        }
    }
}

impl ToGcl for IrInstantiation {
    type Output = GclCommand;

    fn to_gcl(&self, graph: &mut GclGraph, _metadata: &ProgramMetadata) -> Self::Output {
        let loc = graph.get_var_location(self.id);

        // TODO: there should be a lot more happening here

        GclCommand::AddFact(GclFact::HasValue(loc))
    }
}

impl ToGcl for IrAssignment {
    /// The GCL node
    type Output = GclNodeRange;

    fn to_gcl(&self, graph: &mut GclGraph, metadata: &ProgramMetadata) -> Self::Output {
        let lvalue = self.lvalue.to_gcl(graph, metadata);
        let (loc, expr_range) = self.value.to_gcl(graph, metadata);
        let node_name = graph.create_name(&format!("assignment__{}", lvalue));

        let node_idx = graph.add_node(GclNode {
            name: node_name,
            commands: vec![
                GclCommand::AddFact(GclFact::HasValue(lvalue.mem_location())),
                GclCommand::Assignment(GclAssignment {
                    lvalue,
                    expr: GclExpr::var(loc, self.value.ty.clone()),
                }),
            ],
        });

        graph.add_edge(expr_range.end, node_idx, GclExpr::default());

        GclNodeRange {
            start: expr_range.start,
            end: node_idx,
        }
    }
}

impl ToGcl for IrLValue {
    type Output = GclLValue;

    fn to_gcl(&self, graph: &mut GclGraph, metadata: &ProgramMetadata) -> Self::Output {
        match &self.data {
            IrLValueData::Var(var_id) => {
                let loc = graph.get_var_location(*var_id);

                GclLValue::Var(loc)
            }
            IrLValueData::Field(target, field) => {
                let target_lvalue = target.to_gcl(graph, metadata);

                // TODO: If this is a header, do we need to check validity?
                //       Or do that somewhere else, like in field access expr?

                GclLValue::Field(Box::new(target_lvalue), field.clone())
            }
        }
    }
}

impl ToGcl for IrIfStatement {
    type Output = GclNodeRange;

    fn to_gcl(&self, graph: &mut GclGraph, metadata: &ProgramMetadata) -> Self::Output {
        // Create the end node first so we can refer to its index
        let end_node = GclNode {
            name: graph.create_name("if_end"),
            commands: Vec::new(),
        };
        let end_node_idx = graph.add_node(end_node);

        // Calculate the if condition predicate and nodes
        let (condition_loc, cond_range) = self.condition.to_gcl(graph, metadata);
        let pred = GclExpr::var(condition_loc, self.condition.ty.clone());
        let negated_pred = pred.negate();

        // Convert the then and else branches to GCL
        let GclNodeRange {
            start: then_node_start,
            end: then_node_end,
        } = self.then_case.to_gcl(graph, metadata);
        let else_node_range = self
            .else_case
            .as_ref()
            .map(|stmt| stmt.to_gcl(graph, metadata));
        let else_node_idx = if let Some(else_range) = else_node_range {
            else_range.start
        } else {
            end_node_idx
        };

        // Connect the condition nodes to the then & else cases
        graph.add_edge(cond_range.end, then_node_start, pred);
        graph.add_edge(cond_range.end, else_node_idx, negated_pred);

        // Add edges to the end node from then and else branches
        graph.add_edge(then_node_end, end_node_idx, GclExpr::default());
        if let Some(else_range) = else_node_range {
            graph.add_edge(else_range.end, end_node_idx, GclExpr::default());
        }

        GclNodeRange {
            start: cond_range.start,
            end: end_node_idx,
        }
    }
}

impl ToGcl for IrExpr {
    /// The memory location which holds the value of the expression, plus the
    /// nodes that set the value of that memory location.
    type Output = (MemoryLocation, GclNodeRange);

    fn to_gcl(&self, graph: &mut GclGraph, metadata: &ProgramMetadata) -> Self::Output {
        match &self.data {
            IrExprData::Bool(b) => {
                let loc = graph.fresh_mem_location();
                let node_idx = Self::single_assignment_node(graph, loc, GclExpr::bool(*b));

                (
                    loc,
                    GclNodeRange {
                        start: node_idx,
                        end: node_idx,
                    },
                )
            }
            IrExprData::Var(var) => {
                let loc = graph.get_var_location(*var);

                let node = GclNode {
                    name: graph.create_name("expr_var"),
                    commands: Vec::new(),
                };
                let node_idx = graph.add_node(node);
                let assert_idx =
                    make_assert_node(graph, GclExpr::fact(GclFact::HasValue(loc)), node_idx);

                (
                    loc,
                    GclNodeRange {
                        start: assert_idx,
                        end: node_idx,
                    },
                )
            }
            IrExprData::And(left, right) => {
                Self::short_circuit_logic(graph, metadata, left, right, true)
            }
            IrExprData::Or(left, right) => {
                Self::short_circuit_logic(graph, metadata, left, right, false)
            }
            IrExprData::Negation(inner) => {
                let (inner_loc, inner_range) = inner.to_gcl(graph, metadata);
                let inner_pred = GclExpr::var(inner_loc, inner.ty.clone());
                let loc = graph.fresh_mem_location();
                let name = graph.create_name("expr");

                let true_idx = Self::single_assignment_node(graph, loc, GclExpr::bool(true));
                let false_idx = Self::single_assignment_node(graph, loc, GclExpr::bool(false));

                let node_idx = graph.add_node(GclNode {
                    name,
                    commands: Vec::new(),
                });

                graph.add_edge(inner_range.end, true_idx, inner_pred.negate());
                graph.add_edge(inner_range.end, false_idx, inner_pred);
                graph.add_edge(true_idx, node_idx, GclExpr::default());
                graph.add_edge(false_idx, node_idx, GclExpr::default());

                (
                    loc,
                    GclNodeRange {
                        start: inner_range.start,
                        end: node_idx,
                    },
                )
            }
            IrExprData::FunctionCall(func_call) => {
                let func_range = func_call.to_gcl(graph, metadata);
                let loc = graph.fresh_mem_location();
                let node = GclNode {
                    name: graph.create_name("expr_func"),
                    commands: vec![GclCommand::Assignment(GclAssignment {
                        lvalue: GclLValue::Var(loc),
                        expr: GclExpr::var(MemoryLocation::ReturnVal, self.ty.clone()),
                    })],
                };
                let node_idx = graph.add_node(node);
                graph.add_edge(func_range.end, node_idx, GclExpr::default());

                (
                    loc,
                    GclNodeRange {
                        start: func_range.start,
                        end: node_idx,
                    },
                )
            }
            IrExprData::FieldAccess(target, field) => {
                let (target_loc, target_range) = target.to_gcl(graph, metadata);
                let loc = graph.fresh_mem_location();
                let node = GclNode {
                    name: graph.create_name("expr_field_access"),
                    commands: vec![GclCommand::Assignment(GclAssignment {
                        lvalue: GclLValue::Var(loc),
                        expr: GclExpr {
                            ty: self.ty.clone(),
                            data: GclExprData::FieldAccess(
                                Box::new(GclExpr::var(target_loc, target.ty.clone())),
                                field.clone(),
                            ),
                        },
                    })],
                };
                let node_idx = graph.add_node(node);
                graph.add_edge(target_range.end, node_idx, GclExpr::default());

                (
                    loc,
                    GclNodeRange {
                        start: target_range.start,
                        end: node_idx,
                    },
                )
            }
            IrExprData::Struct(fields) => {
                // Convert fields to GCL
                let fields_gcl: Vec<_> = fields
                    .iter()
                    .map(|(key, value)| (key, value.ty.clone(), value.to_gcl(graph, metadata)))
                    .collect();

                // Connect the field initialization nodes
                let fields_range =
                    fields_gcl
                        .iter()
                        .map(|(_, _, (_, range))| *range)
                        .reduce(|acc, next| {
                            graph.add_edge(acc.end, next.start, GclExpr::default());
                            GclNodeRange {
                                start: acc.start,
                                end: next.end,
                            }
                        });

                // Build the struct in GCL
                let fields_gcl = fields_gcl
                    .into_iter()
                    .map(|(key, ty, (value, _))| (key.clone(), GclExpr::var(value, ty)))
                    .collect();
                let loc = graph.fresh_mem_location();
                let node = GclNode {
                    name: graph.create_name("expr_struct"),
                    commands: vec![GclCommand::Assignment(GclAssignment {
                        lvalue: GclLValue::Var(loc),
                        expr: GclExpr {
                            ty: self.ty.clone(),
                            data: GclExprData::Struct { fields: fields_gcl },
                        },
                    })],
                };
                let node_idx = graph.add_node(node);

                // Connect up the field init
                let start_idx = if let Some(fields_range) = fields_range {
                    graph.add_edge(fields_range.end, node_idx, GclExpr::default());
                    fields_range.start
                } else {
                    node_idx
                };

                (
                    loc,
                    GclNodeRange {
                        start: start_idx,
                        end: node_idx,
                    },
                )
            }
        }
    }
}

impl IrExpr {
    /// Create a node which assigns a value to a location
    fn single_assignment_node(
        graph: &mut GclGraph,
        location: MemoryLocation,
        value: GclExpr,
    ) -> NodeIndex {
        let name = graph.create_name("expr");
        graph.add_node(GclNode {
            name,
            commands: vec![GclCommand::Assignment(GclAssignment {
                lvalue: GclLValue::Var(location),
                expr: value,
            })],
        })
    }

    /// The logic for short-circuiting && and || is very similar, so the
    /// implementations are generalized by this function.
    fn short_circuit_logic(
        graph: &mut GclGraph,
        metadata: &ProgramMetadata,
        left: &IrExpr,
        right: &IrExpr,
        is_add: bool,
    ) -> (MemoryLocation, GclNodeRange) {
        let (left_loc, left_range) = left.to_gcl(graph, metadata);
        let (right_loc, right_range) = right.to_gcl(graph, metadata);
        let result_loc = graph.fresh_mem_location();
        let op_name = if is_add { "add" } else { "or" };

        let true_node = GclNode {
            name: graph.create_name(&format!("{}_expr_true", op_name)),
            commands: vec![GclCommand::Assignment(GclAssignment {
                lvalue: GclLValue::Var(result_loc),
                expr: GclExpr::bool(true),
            })],
        };
        let false_node = GclNode {
            name: graph.create_name(&format!("{}_expr_false", op_name)),
            commands: vec![GclCommand::Assignment(GclAssignment {
                lvalue: GclLValue::Var(result_loc),
                expr: GclExpr::bool(false),
            })],
        };
        let end_node = GclNode {
            name: graph.create_name(&format!("{}_expr_end", op_name)),
            commands: Vec::new(),
        };

        let true_node_idx = graph.add_node(true_node);
        let false_node_idx = graph.add_node(false_node);
        let end_node_idx = graph.add_node(end_node);
        let left_pred = GclExpr::var(left_loc, left.ty.clone());
        let right_pred = GclExpr::var(right_loc, right.ty.clone());
        let left_pred_negated = left_pred.negate();
        let right_pred_negated = right_pred.negate();

        let (left_to_right, left_to_set) = if is_add {
            (left_pred, left_pred_negated)
        } else {
            (left_pred_negated, left_pred)
        };

        graph.add_edge(left_range.end, right_range.start, left_to_right);
        graph.add_edge(
            left_range.end,
            if is_add {
                false_node_idx
            } else {
                true_node_idx
            },
            left_to_set,
        );
        graph.add_edge(right_range.end, true_node_idx, right_pred);
        graph.add_edge(right_range.end, false_node_idx, right_pred_negated);
        graph.add_edge(true_node_idx, end_node_idx, GclExpr::default());
        graph.add_edge(false_node_idx, end_node_idx, GclExpr::default());

        (
            result_loc,
            GclNodeRange {
                start: left_range.start,
                end: end_node_idx,
            },
        )
    }
}

impl ToGcl for IrFunctionCall {
    type Output = GclNodeRange;

    fn to_gcl(&self, graph: &mut GclGraph, _metadata: &ProgramMetadata) -> Self::Output {
        // TODO: handle setting arguments and verifying args have values
        let function_range = graph
            .get_function(self.target)
            .unwrap_or_else(|| panic!("Unable to find function {}", self.target));

        let start_name = graph.create_name("func_call_start");
        let end_name = graph.create_name("func_call_end");
        let ret_target_var = graph.fresh_mem_location();
        let start_idx = graph.add_node(GclNode {
            name: start_name,
            commands: vec![GclCommand::Assignment(GclAssignment {
                lvalue: GclLValue::Var(ret_target_var),
                expr: GclExpr::string(end_name.clone()),
            })],
        });
        let end_idx = graph.add_node(GclNode {
            name: end_name.clone(),
            commands: Vec::new(),
        });

        graph.add_edge(start_idx, function_range.start, GclExpr::default());
        graph.add_edge(
            function_range.end,
            end_idx,
            GclExpr::bin_op(
                GclBinOp::Equals,
                GclExpr::var(ret_target_var, IrType::string()),
                GclExpr::string(end_name),
            ),
        );

        GclNodeRange {
            start: start_idx,
            end: end_idx,
        }
    }
}

/// Create an assertion node which, when the predicate is true, jumps to
/// the `next_node`, otherwise jumps to a new "bug" node.
fn make_assert_node(graph: &mut GclGraph, predicate: GclExpr, next_node: NodeIndex) -> NodeIndex {
    let bug_node = GclNode {
        name: graph.create_name("bug"),
        commands: vec![GclCommand::Bug],
    };
    let bug_node_idx = graph.add_node(bug_node);

    let assert_node = GclNode {
        name: graph.create_name("assert"),
        commands: Vec::new(),
    };
    let assert_node_idx = graph.add_node(assert_node);

    graph.add_edge(assert_node_idx, bug_node_idx, predicate.negate());
    graph.add_edge(assert_node_idx, next_node, predicate);

    assert_node_idx
}
