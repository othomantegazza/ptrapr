#' Read Graph of Panicle Structure and Seeds
#'
#' This is a convenient wrapper that read:
#' - A `.ricepr`
#' file (which stores the structure of a panicle).
#' - A a `.ricegr`
#' file (which stores the coordinates of grains).
#'
#' And merges them together
#'
#' @param pr_file The path to the `.ricepr` file,
#'  (which stores the structure of a panicle)
#' @param gr_file The path to the `.ricegr` file,
#'  (which stores the coordinates of grains)
#'
#' @export

read_full_panicle <- function(pr_file,
                              gr_file)
{
  # Read Panicle structure
  panicle <-
    pr_file %>%
    read_panicle()

  # read grain coordinates
  grains <-
    gr_file %>%
    read_grains()

  # Output merged panicle
  full_panicle <-
    panicle %>%
    add_all_grains(grains)

  full_panicle
}
