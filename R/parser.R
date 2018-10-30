#' Read Panicle Spacial Graph
#'
#'
#' Read the XML file that stores panicles architecture
#' data (the P-TRAP output that ends by .ricepr)
#'
#' And returns a `tidygraph` in which each node has
#' spatial coordinates
#'
#' @param path Path to the panicle XML file
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data

read_panicle <- function(path) {
  vertices <- xml2::read_xml(path) %>%
    xml2::xml_find_all(".//vertex")

  nodes <- tibble::tibble(x = vertices %>%
                            xml2::xml_attr("x") %>%
                            as.numeric(),
                          y = vertices %>%
                            xml2::xml_attr("y") %>%
                            as.numeric(),
                          type = vertices %>%
                            xml2::xml_attr("type"),
                          id = vertices %>%
                            xml2::xml_attr("id")) %>%
    dplyr::mutate(rank = 1:n())

  nodes_rank <- purrr::set_names(x = nodes$rank,
                                 nm = nodes$id)

  edges <- xml2::read_xml(path) %>%
    xml2::xml_find_all(".//edge")

  edges <- tibble::tibble(from = edges %>%
                            xml2::xml_attr("vertex1"),
                          to = edges %>%
                            xml2::xml_attr("vertex2")) %>%
    dplyr::mutate(from = nodes_rank[.data$from],
           to = nodes_rank[.data$to]) %>%
    dplyr::mutate_all(unname)

  tst <- tidygraph::tbl_graph(nodes = nodes,
                              edges = edges)
}
