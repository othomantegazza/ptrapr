#' A simple function to Plot Panilces
#'
#' plot_panicle is a simple and convinient wrapper to `ggraph()` to plot and
#' inspect panicle graphs. For versatility, consider calling
#' `ggraph()` directly.
#'
#' @param panicle_graph a panicle graph
#' @param point_size Numeric, the size of the points in the graph. You need
#'     to set this parameter if you want that the edges and the points in the
#'     graph stop overlapping. Defaults to 2 (mm).
#'
#' @export

plot_panicle <- function(panicle_graph,
                         point_size = 1.8)
{
  p <-
    panicle_graph %>%
    ggraph::ggraph(layout = "nicely") +
    ggraph::geom_edge_link(
      arrow = grid::arrow(
        length = grid::unit(0.06, "inches"),
        type = "closed"),
      colour = "grey30",
      end_cap = ggraph::circle(point_size/2, "mm"),
      start_cap = ggraph::circle(point_size/2, "mm")) +
    ggraph::geom_node_point(
      ggplot2::aes_string(colour = "type"),
      size = point_size,
      alpha =.9) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal()

  # print plot
  p
}
