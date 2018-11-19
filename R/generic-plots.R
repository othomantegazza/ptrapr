#' A simple function to Plot Panilces
#'
#' plot_panicle is a simple and convinient wrapper to `ggraph()` to plot and
#' inspect panicle graphs. For versatility, consider calling
#' `ggraph()` directly.
#'
#' @param panicle_graph a panicle graph
#'
#' @export

plot_panicle <- function(panicle_graph)
{
  p <-
    panicle_graph %>%
    ggraph::ggraph(layout = "nicely") +
    ggraph::geom_edge_link(arrow = grid::arrow(length = unit(0.08,
                                                     "inches"),
                                       type = "closed"),
                   colour = "grey30") +
    ggraph::geom_node_point(aes(colour = type),
                    size = 2,
                    alpha =.7) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal()

  # print plot
  p
}
