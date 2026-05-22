#' separate_retracted
#' @description Separates a list of references into retracted and non-retracted articles.
#' @param df A dataframe object of bibliographic data, including an \code{is_retracted}
#'   column as produced by \code{check_retracted()}.
#' @param write_ris Logical. If \code{TRUE}, writes the retracted articles to a RIS file.
#'   Default \code{FALSE}. Set to \code{FALSE} when using inside a Shiny app (handle
#'   file export via \code{downloadHandler} instead).
#' @param filename Name of the output RIS file (without extension) when
#'   \code{write_ris = TRUE}. Default \code{"retracted"}.
#' @param path Directory path for the output file when \code{write_ris = TRUE}.
#'   Default \code{NULL} saves to the working directory.
#'
#' @returns A named list with two dataframes: \code{$retracted} (articles where
#'   \code{is_retracted == 1}) and \code{$non_retracted} (all others).
#' @importFrom dplyr filter %>%
#' @export
#'
#' @examples Add later
separate_retracted <- function(df, write_ris = FALSE, filename = "retracted", path = NULL) {
  retracted_articles <- df %>%
    filter(is_retracted == 1)

  non_retracted <- df %>%
    filter(is_retracted == 0)

  if (write_ris) {
    build_ris(as.data.frame(retracted_articles), save = TRUE, filename = filename, path = path)
  }

  list(retracted = retracted_articles, non_retracted = non_retracted)
}
