#' Extract a Branch from a Panicle Graph
#'
#' This function returns a `igraph` with the branch
#' developing from a selected node.
#'
#' @param panicle A graph of class `igraph`
#' @param vert A number, the vertex of `panicle`` from which to start

pull_branch <- function(panicle,
                        vert)
{
  # The secondary branch is composed by all vertexes
  # downstream the branch node
  sub_verts <-
    panicle %>%
    igraph::subcomponent(vert,
                         mode = "out")

  sub_graph <-
    panicle %>%
    induced_subgraph(sub_verts)
}

#' Make a Tibble with the Vetex ID of the Longest Path
#'
#' This function takes a panicle `igraph` as object and
#' a starting `vertex` as object, finds the longest path
#' that starts from that branch.
#'
#' The longest path is assumend to be the main branch axis
#'
#' The is a returns a `tibble` that
#' stores the ranks of vertexes along that axis and
#' their `type` (Secondary, Spikelet, etc.).
#'
#' @param branch an `igraph` object storing data for a
#'     panicle branch.
#' @param vert_rank a number indicating the `rank` attribute
#'     that identifies the staring vertex.


make_idline <- function(branch,
                        vert_rank)
{
  # since this is a new graph, I have to identify
  # the generating vertex by the attribute rank.
  # Because that attribute is carried in from the
  # original panicle graph
  start_node <-
    branch %>%
    igraph::vertex_attr() %>%
    {which(.$rank == vert_rank)}

  # I assume and that the the end of the branch axis
  # correspond to the furthest vertex, and use this
  # trick to identify it
  top_node <-
    branch %>%
    igraph::distances(v = start_node,
                      mode = "out") %>%
    which.max()

  # and use that node to estimate the path that
  # contains all the nodes on the main axis
  main_path <-
    branch %>%
    igraph::shortest_paths(from = start_node,
                           to = top_node,
                           mode = "out")

  # I am interested in the types of the nodes in the
  # branch...
  node_types <-
    branch %>%
    vertex_attr() %>%
    .$type
  # ...ranked along the nodes in the main axis of the
  # branch
  node_types_ranked <-
    main_path$vpath %>%
    purrr::flatten_int() %>%
    node_types[.]
}
