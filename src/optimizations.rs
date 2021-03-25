use crate::gcl::{GclExpr, GclExprData, GclGraph};
use petgraph::algo::toposort;
use petgraph::visit::EdgeRef;
use petgraph::Direction;
use std::collections::VecDeque;

/// Merge nodes which are just connected by a single "true" edge
pub fn merge_simple_edges(graph: &mut GclGraph) {
    let node_indices = toposort(&**graph, None).expect("There should be no cycles");
    let mut node_indices = VecDeque::from(node_indices);

    while let Some(node_idx) = node_indices.pop_front() {
        if !graph.contains_node(node_idx) {
            // Some nodes in the list may have been merged and don't exist anymore
            continue;
        }

        let edges: Vec<_> = graph
            .edges_directed(node_idx, Direction::Outgoing)
            .collect();

        // Check the out-edges to make sure the nodes can be merged
        let (edge_id, target_idx) = match edges.as_slice() {
            [edge] => {
                let target_in_edge_count = graph
                    .edges_directed(edge.target(), Direction::Incoming)
                    .count();
                if target_in_edge_count != 1
                    || !matches!(
                        edge.weight(),
                        GclExpr {
                            data: GclExprData::Bool(true),
                            ..
                        }
                    )
                {
                    continue;
                }

                (edge.id(), edge.target())
            }
            _ => continue,
        };

        // Remove the "true" edge
        graph.remove_edge(edge_id);

        // Collect the out-edges of the second node
        let out_edges: Vec<_> = graph
            .edges_directed(target_idx, Direction::Outgoing)
            .map(|e| (e.id(), e.target()))
            .collect();

        // Move the out edges from the second node to the first node
        for (id, target) in out_edges {
            let weight = graph.remove_edge(id).unwrap();
            graph.add_edge(node_idx, target, weight);
        }

        // Remove the second node
        let target_node = graph.remove_node(target_idx).unwrap();

        // Update the first node with the second node's commands
        let node = graph.node_weight_mut(node_idx).unwrap();
        node.commands.extend(target_node.commands);

        // Add this node back to the front of the queue because the out edges
        // it gained may also qualify for merging.
        node_indices.push_front(node_idx);
    }
}
