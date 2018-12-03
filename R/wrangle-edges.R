#' Invert Edges on The Main Axis of a Panicle
#'
#' @param panicle_graph a panicle graph
#' @param check_before `logical`, defaults to `FALSE` should the
#'   function check if
#'   the edges on the main axis of the panicle are pointing inward
#'   before inverting them?
#'
#' @export

invert_edges <- function(panicle_graph,
                         check_before = FALSE)
{
  if(check_before) {
    # Detect the base generating vertex as the vertex
    # with type generating with the highest y
    gen <-
      panicle_graph %>%
      igraph::as_long_data_frame() %>%
      dplyr::filter(.data$from_type %in% c("Generating"))

    gen_vert <- gen[which.max(gen$from_x), "from_rank"]

    # being the start point, that vertex is not expected
    # to have incoming edges
    gen_neighbors <-
      panicle_graph %>%
      igraph::neighbors(v = gen_vert, mode = "in")

    if(length(gen_neighbors) == 0) {
      return(panicle_graph)
    }
  }
  # Get the ranks of all the nodes on the primary axis
  # the primary axis is defined as all the edges that
  # start from a vertex with type "Primary" or "Generating"
  # and end on a vertex again with any of the aforementioned
  # types
  prim_gen <-
    panicle_graph %>%
    igraph::as_long_data_frame() %>%
    dplyr::filter(.data$from_type %in% c("Primary", "Generating"),
                  .data$to_type %in% c("Primary", "Generating")) %>%
    dplyr::select(.data$from, .data$to)

  # tbl_graph has the very convinient method activate()
  panicle_graph %>%
    tidygraph::as_tbl_graph() %>%
    tidygraph::activate("edges") %>%
    tidygraph::reroute(
      from = dplyr::case_when(from %in% prim_gen$from &
                                to %in% prim_gen$to ~ to,
                              TRUE ~ from),
      to = dplyr::case_when(from %in% prim_gen$from &
                       to %in% prim_gen$to ~ from,
                     TRUE ~ to)
      )
}
