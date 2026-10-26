#' Title Randomized Complete Design with One Factor
#'
#' @param fct_names A character vector of length two specifying the name of the treatment factor and the name of the blocks
#' @param treatments A character vector of treatment levels
#' @param blocks A character vector of block levels
#' @param reps Number of replications
#' @param seed Random number seed
#' @returns A data frame with three columns and (treatment levels x block levels x reps) rows
#' @export
#'
#' @examples
#' design4 <- rcb_dsn(
#' fct_names = c("Nitrogen", "Plot"),
#' treatments = LETTERS[1:6],
#' blocks = as.character(1:4),
#' reps = 1)
#'
dsgn_rcb <- function(fct_names = c("Treatment", "Block"),
                     treatments,
                     blocks,
                     reps,
                     seed = 42) {
  set.seed(seed)
  # 1. Create the base combinations of treatments and blocks
  dfx <- expand.grid(Treatment = treatments, Block = blocks,
                     KEEP.OUT.ATTRS = FALSE)
  # 2. Replicate the combinations, group by block, and shuffle the treatments
  dfx <- dfx |>
    dplyr::group_by(Block) |>
    dplyr::mutate(k = reps) |>
    tidyr::uncount(k) |>
    dplyr::slice_sample(prop = 1) |>
    # Randomize row order within each Block
    dplyr::mutate(Run_Order = dplyr::row_number()) |>
    # Add 1, 2, 3... per block
    dplyr::ungroup()    # Remove grouping before returning
  # dplyr::select(fct_names[2] = Block, fct_names[1] = Treatment, "Run")
  colnames(dfx)[1:2] <- fct_names
  return(dplyr::select(dfx, c(2, 3, 1)))
  }

