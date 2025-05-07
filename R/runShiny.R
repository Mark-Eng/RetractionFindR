#' A wrapper function to run the Shiny App 
#' @return A shiny app
#' 
#' @import shiny
#' @import retractionfindr
#' @import synthesisr
#' @import tidyverse
#' @import shinyBS
#' 
#' @export

runShiny <- function(){
  
  # find and launch the app
  appDir <- system.file("RetractionFindR", package = "retractionfindr")
  
  shiny::runApp(appDir, display.mode = "normal")
}