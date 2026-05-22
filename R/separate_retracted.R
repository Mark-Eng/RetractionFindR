#' separate_retracted
#' @description Separates a list of references into retracted and non-retracted articles.
#' @param df A dataframe object of bibliographic data, including an \code{is_retracted}
#'   column as produced by \code{check_retracted()}.
#' @param write_ris Logical. If \code{TRUE}, writes the retracted articles to a RIS file
#'   in the working directory. Default \code{FALSE}. Set to \code{FALSE} when using inside
#'   a Shiny app (handle file export via \code{downloadHandler} instead).
#'
#' @returns A named list with two dataframes: \code{$retracted} (articles where
#'   \code{is_retracted == 1}) and \code{$non_retracted} (all others).
#' @importFrom dplyr filter %>%
#' @importFrom synthesisr write_refs
#' @export
#'
#' @examples Add later
separate_retracted <- function(df, write_ris = FALSE) {
  retracted_articles <- df %>%
    filter(is_retracted == 1)

  non_retracted <- df %>%
    filter(is_retracted == 0)

  if (write_ris) {
    synthesisr::write_refs(as.data.frame(retracted_articles), format = "ris")
  }

  list(retracted = retracted_articles, non_retracted = non_retracted)
}
