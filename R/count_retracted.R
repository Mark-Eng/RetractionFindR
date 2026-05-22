
#' count_retracted
#'
#' @description Counts retracted and total references in a dataframe produced
#'   by \code{check_retracted()}.
#'
#' @param df A dataframe with an \code{is_retracted} column (1 = retracted, 0 = not).
#'
#' @returns A named numeric vector with elements \code{n_retracted} and
#'   \code{n_total}. Prints readably at the console; individual values can be
#'   accessed by name (e.g. \code{counts["n_retracted"]}) for use in Shiny
#'   \code{valueBox} or other programmatic contexts.
#' @export
#'
#' @examples Add later
count_retracted <- function(df) {
  c(n_retracted = sum(df$is_retracted), n_total = nrow(df))
}
