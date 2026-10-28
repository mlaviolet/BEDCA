#' Generate Completely Randomized Design with Two Factors
#'
#' @param fct_names A character vector of length two specifying the names of the factors
#' @param A_levels A character vector specifying the levels of factor A
#' @param B_levels A character vector specifying the levels of factor B
#' @param reps Nummber of replications
#' @param seed Random number seed
#'
#' @returns A data frame with three columns and (A levels x B levels x reps) rows
#' @importFrom dplyr mutate slice_sample row_number
#' @importFrom tidyr uncount
#' @importFrom utils globalVariables
#'
#' @examples
#' design2 <- dsgn_two_way(fct_names = c("concentration", "time"),
#'                         A_levels = c("50%", "75%", "100%"),
#'                         B_levels = c("30 sec", "60 sec", "120 sec"),
#'                         reps = 2)
#'
dsgn_two_way <- function(fct_names, A_levels, B_levels, reps, seed = 42) {
  set.seed(seed)
  dfx <- expand.grid(A_levels, B_levels, KEEP.OUT.ATTRS = FALSE)
  colnames(dfx) <- fct_names
  dfx <- dfx |>
    dplyr::mutate(k = reps) |>
    tidyr::uncount(.data$k) |>
    dplyr::slice_sample(prop = 1) |>        # shuffle rows in random order
    dplyr::mutate(Run_Order = dplyr::row_number()) # add run order
  return(dfx)
  }

# Clear the R CMD check notes for non-standard evaluation variables
utils::globalVariables(c(".data", "k", "Run_Order"))
