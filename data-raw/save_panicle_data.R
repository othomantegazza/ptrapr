library(ptrapr)
library(ggraph)
library(magrittr)
library(igraph)
# library(tibble)
library(dplyr)
# library(tidygraph)



# # Read Panicle XML file ---------------------------------------------------
#
# # Read Panicle structure
# path <- system.file("extdata/Nip_3_3_6315.ricepr",
#                     package = "ptrapr")
#
# panicle <-
#   path %>%
#   read_panicle()
#
# # read grain coordinates
# grain_path <- system.file("extdata/Nip_3_3_6315.ricegr",
#                           package = "ptrapr")
#
# grains <-
#   grain_path %>%
#   read_grains()
#
#
# # Merge grains into structure ---------------------------------------------
#
# full_panicle_nipp <-
#   panicle %>%
#   add_all_grains(grains)
#
#
# # Check that the panicle graph is fine ------------------------------------
#
# if(FALSE) {
#   full_panicle_nipp %>%
#     ggraph() +
#     geom_edge_link(arrow = grid::arrow(length = unit(0.08,
#                                                      "inches"),
#                                        type = "closed"),
#                    colour = "grey30") +
#     geom_node_point(aes(colour = type),
#                     size = 2,
#                     alpha =.7) +
#     coord_fixed() +
#     theme_minimal()
# }
#
#
# # Save panicle nipp -------------------------------------------------------
#
# use_data(full_panicle_nipp)


# Read good panicle file --------------------------------------------------

good_p_path <- list(
  stucture = system.file("extdata/Nip_3_3_6315.ricepr",
                         package = "ptrapr"),
  grains = system.file("extdata/Nip_3_3_6315.ricegr",
                       package = "ptrapr")
)

good_panicle <-
  good_p_path %>%
  {read_full_panicle(
    pr_file = .$stucture,
    gr_file = .$grains
  )}


# Read Panicle with inverted edges ----------------------------------------

inverted_p_path <- list(
  stucture = system.file("extdata/Nip_1_1_6307.ricepr",
                         package = "ptrapr"),
  grains = system.file("extdata/Nip_1_1_6307.ricegr",
                       package = "ptrapr")
)

inverted_panicle <-
  inverted_p_path %>%
  {read_full_panicle(
    pr_file = .$stucture,
    gr_file = .$grains
  )}

# Check that the panicle graphs are fine ------------------------------------

plot_p <- function(p)
  {
  p %>%
    ggraph() +
    geom_edge_link(arrow = grid::arrow(length = unit(0.08,
                                                     "inches"),
                                       type = "closed"),
                   colour = "grey30") +
    geom_node_point(aes(colour = type),
                    size = 2,
                    alpha =.7) +
    coord_fixed() +
    theme_minimal()

  p
}

if(FALSE) {
  plot_p(good_panicle)
  plot_p(inverted_panicle)
}

use_data(good_panicle, inverted_panicle)
