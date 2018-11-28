library(ptrapr)
library(ggraph)
library(magrittr)
library(igraph)
library(dplyr)

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


if(FALSE) {
  plot_panicle(good_panicle)
  plot_panicle(inverted_panicle)
}

usethis::use_data(good_panicle, inverted_panicle, overwrite = TRUE)
