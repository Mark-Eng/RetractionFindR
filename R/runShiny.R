#' Run the RetractionFindR Shiny app
#'
#' @description Launches the RetractionFindR Shiny web application, which
#'   provides a browser-based interface for uploading a .ris file and checking
#'   it against the RetractionWatch database.
#'
#' @returns Launches the Shiny app; does not return a value.
#' @import shiny
#' @export
#'
#' @examples
#' \dontrun{
#' runShiny()
#' }
runShiny <- function() {
  appDir <- system.file("RetractionFindR", package = "retractionfindr")
  shiny::runApp(appDir, display.mode = "normal")
}
