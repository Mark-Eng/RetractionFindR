
#' check_retracted
#' @description
#' Checks a list of references for DOIs/titles that appear in the RetractionWatch
#' database, adding an \code{is_retracted} column (1 = retracted, 0 = not retracted).
#'
#' @param refs A dataframe containing bibliographic references. Any columns not
#'   required for matching are preserved in the returned dataframe.
#' @param retraction_data Optional. A dataframe as returned by
#'   \code{load_retraction_data()}. If \code{NULL} (default), the bundled local
#'   copy of the RetractionWatch data is loaded automatically. Pass a pre-loaded
#'   dataframe to avoid reloading on every call — recommended for Shiny apps,
#'   where \code{load_retraction_data()} should be called once at startup.
#'
#' @returns The input dataframe with an additional \code{is_retracted} column
#'   (1 if the article appears in RetractionWatch, 0 otherwise). All original
#'   columns are preserved.
#' @importFrom dplyr mutate case_when %>%
#' @importFrom stringr str_remove_all str_to_lower
#' @export
#'
#' @examples Add later.
check_retracted <- function(refs, retraction_data = NULL) {

  # Convert all 'NA's to NAs
  refs[refs == "NA"] <- NA

  # Ensure required columns exist (add as NA if missing, but keep all other cols)
  required_cols <- c('source_type', 'author', 'year', 'title', 'journal',
                     'volume', 'issue', 'start_page', 'end_page',
                     'abstract', 'doi', 'publisher')
  for (col in required_cols) {
    if (is.null(refs[[col]])) refs[[col]] <- NA
  }

  # Load retraction data if not supplied
  if (is.null(retraction_data)) {
    retraction_data <- load_retraction_data()
  }

  # Find retracted articles by DOI or title
  refs <- refs %>%
    mutate(clean_title = str_remove_all(str_to_lower(title), "[[:punct:]]")) %>%
    mutate(is_retracted = case_when(
      doi %in% c(retraction_data$OriginalPaperDOI, retraction_data$RetractionDOI) ~ 1,
      clean_title %in% retraction_data$clean_title ~ 1,
      TRUE ~ 0
    ))

  refs$clean_title <- NULL

  return(refs)
}
