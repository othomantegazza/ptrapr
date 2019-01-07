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

#' Make a Tibble with the Vertex ID of the Longest Path
#'
#' This function takes a branch `igraph` as object and
#' a starting `vertex` as object, finds the longest path
#' that starts from that vertex.
#'
#' The longest path is assumend to be the main branch axis
#'
#' This function returns a character vector that
#' stores `type` of the vertexes (Secondary, Spikelet, etc.)
#' along that axis ordered from the starting vertex
#' (supplied in `vert_rank`) going outward.
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

  # I assume and that the end of the branch axis
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
  # and in their original rank
  node_types <-
    branch %>%
    igraph::vertex_attr() %>%
    {dplyr::tibble(type = .$type, original_rank = .$rank)}

  # ...ranked along the nodes in the main axis of the
  # branch
  # this returns a character vector of ordered nodes
  out <-
    main_path$vpath %>%
    purrr::flatten_int() %>%
    node_types[., ] %>%
    dplyr::mutate(branchwise_rank = main_path$vpath %>%
             purrr::flatten_int())

  # get the nodes that are downstream
  # secondary branches and not in the
  # main path
  get_down_secondary <- function(vert_id) {

    # the ids of vertexes direcltly downstream
    down_vert_ids <-
      igraph::neighbors(graph = branch,
                        v = vert_id,
                        mode = "out")

    # the number of vertexes downstream
    verts_down <- 0

    for(i in down_vert_ids) {
      if(! i %in% vpath){
        subcomps <-
          branch %>%
          igraph::subcomponent(i,
                               mode = "out") %>%
          length()

        verts_down <- verts_down + subcomps
      }
    }
    return(verts_down)
  }

  out <-
    out %>%
    dplyr::mutate(nodes_downstream = list(
      igraph::neighbors(
        graph = branch,
        v = .data$branchwise_rank,
        mode = "out")
    ))

  # scaffold?
  vpath <- main_path$vpath %>%
    purrr::flatten_int()

  # Save nodes downstream in the output of out
  out <-
    out %>%
    dplyr::mutate(nodes_downstream = .data$branchwise_rank %>%
                    purrr::map_dbl(get_down_secondary))

  return(out)
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
    dplyr::pull(.data$from_rank)
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
#' @param silently logical, if `FALSE` prints intermediate data
#'     while producing the panicle tibble. Defaults to `TRUE`.
#'
#' @export

panicle_tibble <- function(panicle,
                           silently = TRUE)
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

    nb[keep] %>% as.numeric()
  }

  # All nodes where a primary branch starts
  branch_starts <-
    main_path %>%
    purrr::map(not_primary)

  # sometimes two branches start from the same node
  # in case, First, print a note
  if(!silently) {
    double_branches <-
      branch_starts %>%
      purrr::map_dbl(length) %>%
      {which(. > 1)}
    if(length(double_branches > 0))
      paste("Multiple branches detected on node:", double_branches,
            "\n This node was split in multiple nodes") %>%
      message()
  }
  # Then solve it by arbitrarily splitting that node
  # in two
  branch_starts <-
    branch_starts %>%
    purrr::flatten_dbl()

  # pull a subgraph from every primary node
  tb <- tibble::tibble(vert_rank = branch_starts,
                       branch = branch_starts %>%
                         purrr::map(
                           ~pull_branch(panicle = panicle,
                                        vert = .))
    )

  if(!silently) {print(tb)}

  # new version
  tb_list <-
    tb %>%
    purrr::pmap(make_idline) %>%
    purrr::map(
      ~dplyr::mutate(., node_rank = 1:dplyr::n())
      )

  tb_list <-
    1:length(tb_list) %>%
    purrr::map(~dplyr::mutate(tb_list[[.]], primary_rank = .)) %>%
    purrr::reduce(dplyr::bind_rows)

  # Two columns: original_rank and branchwise_rank are confusing
  # and are useful only for internal calculations:
  # remove them from the final output
  tb_list <-
    tb_list %>%
    dplyr::select(-.data$original_rank, -.data$branchwise_rank) %>%
  # and rename one in a clearer way
    dplyr::rename(secondary_rank = "node_rank")
}

#' Plot the Output of `panicle_tibble()`
#'
#' Returns a `ggplot2` object.
#'
#' This is a small utility plot function. You can use it
#' on the output of `panicle_tibble()` to represent it as a
#' tileplot.
#'
#' Although we provide this function as utility, we suggest that you design
#' your own plotting function, because only in this way you will reach the
#' versatlity required for exploratory data analysis. We provide ideas
#' on how to achieve this in the vignettes.
#'
#' This tileplot is inspired by the plots in
#' https://www.nature.com/articles/s41598-018-30395-9
#'
#' @param pan_tbl A tibble, the output of `panicle_tibble()`
#' @param draw.plot Logical. Should the function draw a plot on the graphic device?
#'     Defaults to `FALSE`.
#'
#' @export

panicle_tileplot <- function(pan_tbl, draw.plot = FALSE)
{
  p <-
    pan_tbl %>%
    ggplot2::ggplot(
      ggplot2::aes_string(
        x = "secondary_rank",
        y = "primary_rank",
        fill = "type")) +
    ggplot2::geom_tile(
      colour = "grey80",
      size = 2) +
    ggplot2::geom_text(data = . %>%
                dplyr::filter(.data$type == "Seconday"),
                ggplot2::aes_string(label = "nodes_downstream"),
                colour = "grey20")

  if(draw.plot) print(p)

  return(p)
}

#' Plot the Output of `panicle_tibble()`
#'
#' This function is the same as `panicle_tileplot()`, with extra customization
#' of the output plot.
#'
#' Returns a `ggplot2` object.
#'
#' This is a small utility plot function. You can use it
#' on the output of `panicle_tibble()` to represent it as a
#' tileplot.
#'
#' Although we provide this function as utility, we suggest that you design
#' your own plotting function, because only in this way you will reach the
#' versatlity required for exploratory data analysis. We provide ideas
#' on how to achieve this in the vignettes.
#'
#' This tileplot is inspired by the plots in
#' https://www.nature.com/articles/s41598-018-30395-9
#'
#' @param pan_tbl A tibble, the output of `panicle_tibble()`
#' @param draw.plot Logical. Should the function draw a plot on the graphic device?
#'     Defaults to `FALSE`.
#'
#' @export

panicle_tileplot2 <- function(pan_tbl, draw.plot = FALSE)
{
  p <-
    pan_tbl %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = secondary_rank %>% as.character() %>% as_factor(),
        y = primary_rank %>% as.character() %>% as_factor(),
        fill = nodes_downstream)) + # nodes on 2ndary branches to fill colours +
    ggplot2::geom_tile(colour = "grey80",
               size = 1.5) +
    ggplot2::geom_text(data = . %>%
                dplyr::filter(type == "Seconday"), # Show the number for secondary nodes
              ggplot2::aes(label = nodes_downstream),
              colour = "grey30",
              fontface = "bold",
              size = 5) +
    # fixed xy ratio, each tile is a square
    ggplot2::coord_fixed() +
    # set scale of colours and
    # proper labels for the colour scales
    ggplot2::scale_fill_viridis_c(
      breaks = function(limits) c(0, 2:max(limits)),
      labels = function(breaks) dplyr::case_when(breaks == 0 ~ "1\n[Spikelet]",
                                                 TRUE ~ as.character(breaks)),
      guide = ggplot2::guide_legend(nrow = 1,
                                    # keyheight = unit(7, units = "mm"),
                                    # keywidth = unit(7, units = "mm"),
                                    override.aes = list(size = 0))) +
    # fill guide on top
    ggplot2::theme(legend.position = "top") +
    # Clear axis names
    ggplot2::labs(x = "Nodes rank along primary branches",
         y = "Rank along the rachis",
         fill = "Nodes on\n secondary branch")
  if(draw.plot) print(p)

  return(p)
}
