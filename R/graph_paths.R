#' Visualise dose-paths as a graph
#'
#' @details
#' The viridis package supports palettes: viridis, magma, plasma, inferno, and
#' cividis. The RColorBrewer package supports many palettes. Refer to those
#' packages on CRAN for more details.
#'
#' @param paths Object of type \code{\link{dose_paths}}
#' @param viridis_palette optional name of a colour palette in the
#' viridis package.
#' @param RColorBrewer_palette optional name of a colour palette in the
#' RColorBrewer package.
#'
#' @importFrom tibble tibble as_tibble
#' @importFrom viridis viridis
#' @importFrom RColorBrewer brewer.pal
#' @importFrom magrittr %>%
#' @importFrom dplyr transmute left_join filter select mutate
#' @importFrom DiagrammeR create_graph render_graph
#' @export
#'
#' @examples
#' paths <- get_three_plus_three(num_doses = 5) %>%
#'   get_dose_paths(cohort_sizes = c(3, 3, 3))
#'
#' graph_paths(paths)
#' graph_paths(paths, viridis_palette = 'plasma')
#' graph_paths(paths, RColorBrewer_palette = 'YlOrRd')
graph_paths <- function(paths,
                        viridis_palette = 'viridis',
                        RColorBrewer_palette = NULL
) {

  .node <- .parent <- mod <- next_dose <- outcomes <- NULL

  stop_label <- 'Stop'
  df <- as_tibble(paths)

  num_colours <- num_doses(paths) + 1

  if(is.null(RColorBrewer_palette)) {
    df_colour <- tibble(
      dose = c(stop_label, as.character(dose_indices(paths))),
      fillcolor = viridis(num_colours, option = viridis_palette)
    )
  } else {
    df_colour <- tibble(
      dose = c(stop_label, as.character(dose_indices(paths))),
      fillcolor = brewer.pal(num_colours, RColorBrewer_palette)
    )
  }

  col_offset <- as.integer(num_colours / 2)
  i <- 1 + mod((seq_along(df_colour$fillcolor) + col_offset - 1), num_colours)
  df_colour$fontcolor <- df_colour$fillcolor[i]

  df %>%
    transmute(id = .node,
              type = NA,
              next_dose,
              label = case_when(
                is.na(next_dose) ~ 'Stop',
                TRUE ~ next_dose %>% as.character())
    ) %>%
    left_join(df_colour, by = c('label' = 'dose')) -> ndf

  df %>%
    filter(!is.na(.parent)) %>%
    select(from = .parent, to = .node, label = outcomes) %>%
    mutate(rel = "leading_to") -> edf

  graph <- create_graph(nodes_df = ndf, edges_df = edf)
  render_graph(graph)
}
