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
    igraph::induced_subgraph(sub_verts)
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
    igraph::vertex_attr() %>%
    .$type
  # ...ranked along the nodes in the main axis of the
  # branch
  node_types_ranked <-
    main_path$vpath %>%
    purrr::flatten_int() %>%
    node_types[.]
}

#' Identify Start and End Generating node
#'
#' This function select the start generating node
#' as the one with the highest x coordinate and the
#' end as the node with the lowest
#'
#' This approach is prone to error is the panicle
#' is not orientated from right to left in the
#' P-TRAP picture.
#'
#' This function is used and improved by `get_base()`,
#' which select the base node as the generating
#' node with no parents
#'
#' @param panicle a panicle graph.

get_generating <- function(panicle) {
  panicle %>%
    igraph::as_long_data_frame() %>%
    dplyr::filter(.data$from_type == "Generating") %>%
    dplyr::select(.data$from_x, .data$from_rank) %>%
    dplyr::distinct() %>%
    dplyr::arrange(
      dplyr::desc(.data$from_x)
      ) %>%
    dplyr::pull(from_rank)
}

#' Get the node at the base of the panicle
#'
#' This function returns the rank of the node (vertex) at the
#' base of the panicle. We assume that the base node
#' is the node  with type: `Generating` with no
#' children
#'
#' @param panicle a panicle graph


get_base <- function(panicle) {
  # the base node is one of the generating
  gens <- get_generating(panicle)
  # that have no parent nodes in its neighbors
  parents <-
    gens %>%
    purrr::map(
      ~igraph::neighbors(panicle,
                         v = .,
                         mode = "in")
      )

  gens[which(
    parents %>% purrr::map_int(length) == 0
    )]
}

#' Turn a Panicle Graph in a Tibble useful for a dotplot
#'
#' @param panicle a panicle graph.
#'
#' @export

panicle_tibble <- function(panicle)
  {
  # get the generating node
  gens <- get_generating(panicle)

  # the base node is the one with no parents
  start <- get_base(panicle)

  # the other is the top node
  to <- gens[which(gens != start)]

  # we assume that the main axis of the rachis
  # is the shortest (and only!) path between
  # the two generating nodes
  main_path <-
    panicle %>%
    igraph::shortest_paths(from = start,
                           to = to,
                           mode = "out") %>%
    .$vpath %>%
    purrr::flatten_int()

  # I need a way to select neighbours
  # with any other attribute than primary
  not_primary <- function(v) {
    #

    # get the neighborough nodes
    nb <- igraph::neighbors(graph = panicle,
                            v = v,
                            mode = "out")
    # and its attributes
    nb_attr <- igraph::vertex_attr(graph = panicle,
                                   index = nb)
    keep <- nb$type != "Primary" & nb$type != "Generating"
    # scaffold
    nb[keep] %>% as.numeric() %>% print()

    nb[keep] %>% as.numeric()
  }

  branch_starts <-
    main_path %>%
    purrr::map(not_primary) # %>%
    # Kludge!!!!!!
    # .[1:9]

  # scaffold
  # return(branch_starts)


  #scaffold
  # tb

  tb <- tibble::tibble(vert_rank = branch_starts,
                       branch = branch_starts %>%
                         purrr::map(
                           ~pull_branch(panicle = panicle,
                                        vert = .))
    )

  # scaffold
  # print("a")
  # return(tb)

  tb_list <-
    tb %>%
    purrr::pmap(make_idline) #%>% scaffold
    # kludge
    # .[-3] %>%

  # scaffold
  return(tb_list)

    purrr::map(
      ~tibble::tibble(type = .,
              node_rank = 1:length(.))
    )

  tb_list <-
  1:length(tb_list) %>%
    purrr::map(~dplyr::mutate(tb_list[[.]], primary_rank = .))

  tb_list %>% purrr::reduce(dplyr::bind_rows)


  # purrr::map(
  #   ~make_idline()
  #   )
  # main_path %>%
  #   purrr::map(
  #     ~igraph::neighbors(graph = panicle,
  #                        v = .,
  #                        mode = "out")
  #     ) %>%
  #   map(
  #     ~vertex_attr(graph = panicle,
  #                  index = .)
  #     )
}
