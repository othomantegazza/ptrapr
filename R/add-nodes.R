#' Add a Grain to the Closest Branch
#'
#'
#' @param panicle_graph A panicle `tidygraph` object
#' @param new_vert Numeric vector that stores the coordinates
#'      of the new grain in format `c(x, y)`
#'
#' @export


add_grain <- function(panicle_graph,
                         new_vert = numeric(2))
{

  # new_vert in format c(x, y)

  # Long datagrame easier to subset than graph
  graph_long <- panicle_graph %>% igraph::as_long_data_frame()

  # function
  get_distance <- function(x0, y0,
                           x1, y1,
                           x2, y2)
  {
    ln <- sp::Line(matrix(c(x1, x2,
                            y1, y2),
                          ncol = 2)) %>%
      sp::Lines(., ID = "a") %>%
      list(.) %>%
      sp::SpatialLines()
    pn <- sp::SpatialPoints(coords = matrix(c(x0, y0),
                                            ncol = 2))
    # print(ln)
    # print(pn)
    return(rgeos::gDistance(ln, pn))
  }

  # test
  # get_distance(1328,  712, 1284,  688, 1329,  579)

  # Find edge rank
  nearest_edge <- graph_long %>%
    purrr::pmap(.,
         ~get_distance(x0 = new_vert[1],
                       y0 = new_vert[2],
                       x1 = ..3,
                       y1 = ..4,
                       x2 = ..8,
                       y2 = ..9)) %>%
    purrr::reduce(c) %>%
    which.min()

  # count edges
  vn <- igraph::vcount(panicle_graph)
  new_vn <- vn + 1

  # add spikelet and new edges
  new_graph <- panicle_graph %>%
    igraph::delete_edges(edges = paste0(as.numeric(graph_long[nearest_edge, "from"]),
                                        "|",
                                        as.numeric(graph_long[nearest_edge, "to"]))) %>%
    igraph::add_vertices(nv = 1,
                         attr = list(x = new_vert[1],
                                     y = new_vert[2],
                                     type = "spikelet")) %>%
    # igraph::as_long_data_frame()
    igraph::add_edges(edges = c(as.numeric(graph_long[nearest_edge, "from"]), new_vn,
                                new_vn, as.numeric(graph_long[nearest_edge, "to"])))


  # the new vertexes need a rank
  # for the function get_idline to work
  vattr <- igraph::vertex_attr(new_graph)
  vattr$rank[is.na(vattr$rank)] <- (max(vattr$rank, na.rm = T) + 1):length(vattr$rank)
  igraph::vertex_attr(new_graph) <- vattr

  return(new_graph)
}

#' Merge all grains into graph
#'
#' @export

add_all_grains <- function(graph_base, grains)
{
  # spikelets <- seeds
  # graph_base <- tst
  assign_grain <- function(graph_base, grain) {
    graph_base <<- add_grain(graph_base, grain)
  }
  purrr::pmap(grains, ~assign_grain(graph_base = graph_base,
                                   grain = c(..1, ..2)))

  return(graph_base)
}
