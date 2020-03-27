
dose_finding_path_node <- function(node_id, parent_node_id, depth, outcomes,
                                   next_dose, fit, parent_fit) {
  x <- list(.node = node_id,
            .parent = parent_node_id,
            .depth = depth,
            outcomes = outcomes,
            next_dose = next_dose,
            fit = fit,
            parent_fit = parent_fit)
  class(x) <- c("dose_finding_path_node")
  x
}
