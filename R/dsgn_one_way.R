#' Generate Completely Randomized Design with One Factor
#'
#' @param factor_name A character vector of length one specifying the name of the factor
#' @param levels A character vector specifying the levels of the factor
#' @param reps Number of replications
#' @param seed Random number seed
#'
#' @returns A data frame with two columns and (levels x reps) rows
#' @importFrom dplyr mutate slice_sample row_number
#' @importFrom tidyr uncount
#' @importFrom utils globalVariables
#' @export
#'
#' @examples
#' design1 <- dsgn_one_way("Treatment", c("a", "b", "c"), 10)
#'
dsgn_one_way <- function(factor_name, levels, reps, seed = 42) {
  set.seed(seed)
  dfx <- data.frame(
    trtmnt = factor(levels),                 # define treatment levels
    k = reps) |>                             # run k replications each level
    tidyr::uncount(k) |>               # make k copies of each row
    dplyr::slice_sample(prop = 1) |>         # shuffle rows in random order
    dplyr::mutate(Run_Order = dplyr::row_number())  # add run order
  colnames(dfx)[2] <- factor_name         # change treatment column to user name
  return(dfx)
  }

# Clear the R CMD check notes for non-standard evaluation variables
utils::globalVariables(c(".data", "k", "Run_Order"))
