#' Load RetractionWatch data for use in check_retracted
#'
#' @description Reads the RetractionWatch CSV and returns a trimmed, matching-ready
#'   dataframe containing only the columns needed for retraction checking
#'   (\code{Title}, \code{OriginalPaperDOI}, \code{RetractionDOI}) plus a
#'   pre-computed \code{clean_title} column. Loading only these columns keeps
#'   the in-memory footprint small (~10 MB vs ~400 MB for the full CSV), which
#'   matters for Shiny deployments.
#'
#' @param path Path or URL to the RetractionWatch CSV. Defaults to the bundled
#'   local copy included with the package. To always use the latest data without
#'   redeploying your Shiny app, pass the Crossref URL directly:
#'   \code{"https://gitlab.com/crossref/retraction-watch-data/-/raw/main/retraction_watch.csv?ref_type=heads"}.
#'   In a Shiny app, call this once in \code{global.R} and pass the result to
#'   \code{check_retracted()} via its \code{retraction_data} argument.
#'
#' @returns A dataframe with columns \code{Title}, \code{OriginalPaperDOI},
#'   \code{RetractionDOI}, and \code{clean_title}.
#' @importFrom stringr str_remove_all str_to_lower
#' @export
#'
#' @examples Add later.
load_retraction_data <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("data", "retraction_watch.csv", package = "retractionfindr")
  }

  cols_needed <- c("Title", "OriginalPaperDOI", "RetractionDOI")
  header <- names(read.csv(path, nrow = 0))
  col_classes <- ifelse(header %in% cols_needed, NA, "NULL")

  retracted <- read.csv(path, colClasses = col_classes)
  retracted$clean_title <- str_remove_all(str_to_lower(retracted$Title), "[[:punct:]]")
  retracted
}


#' Update the bundled RetractionWatch data
#'
#' @description Downloads the latest RetractionWatch CSV from Crossref and saves
#'   it to the package data directory, replacing the bundled local copy. The
#'   GitHub Actions workflow calls this automatically on a schedule; use this
#'   function to trigger a manual update or from a Shiny admin panel.
#'
#' @returns Invisibly returns the path to the saved file.
#' @export
#'
#' @examples Add later.
update_retraction_data <- function() {
  url <- "https://gitlab.com/crossref/retraction-watch-data/-/raw/main/retraction_watch.csv?ref_type=heads"
  retwatch_db <- read.csv(url)

  out_dir <- system.file("data", package = "retractionfindr")
  write.csv(retwatch_db, file = file.path(out_dir, "retraction_watch.csv"))
  cat("Retraction Watch data last retrieved on", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      file = file.path(out_dir, "latest_update.txt"))

  invisible(file.path(out_dir, "retraction_watch.csv"))
}
