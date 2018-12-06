#' Read Graph of Panicle Structure and Seeds
#'
#' This is a convenient wrapper that read:
#' - A `.ricepr`
#' file (which stores the structure of a panicle).
#' - A a `.ricegr`
#' file (which stores the coordinates of grains).

#' And merges them together
#'
#' If the name of the `.ricepr` and the `.ricegr` is the same,
#' you can provide this name without extension to the parameter
#' `base_path`. Otherwise, you can provide the path to the two
#' files with their extensions to the parameters
#'`pr_file` and `gr_file` .
#'
#' @param base_path The path to the panicle files without extension.
#'     Use this parameter only if the names of the panicle structure file
#'     and of the panicle structure files are the same, and they are in the
#'     same folder. The `base_path` parameter and the `pr_file` + `gr_file`
#'     parameters are mutually exclusive. Use the first, or the others.
#' @param pr_file The path to the `.ricepr` file,
#'  (which stores the structure of a panicle)
#' @param gr_file The path to the `.ricegr` file,
#'  (which stores the coordinates of grains)
#'
#' @export

read_full_panicle <- function(base_path = NULL,
                              pr_file,
                              gr_file)
{
  if(!is.null(base_path)) {
    pr_file <- paste0(base_path, ".ricepr")
    gr_file <- paste0(base_path, ".ricegr")
  }
  # Read Panicle structure
  panicle <-
    read_panicle(path = pr_file)

  # read grain coordinates
  grains <-
    read_grains(path = gr_file)

  # Output merged panicle
  full_panicle <-
    panicle %>%
    add_all_grains(grains)

  full_panicle
}
