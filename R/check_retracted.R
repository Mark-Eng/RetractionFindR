
#' check_retracted
#'
#' @param refs A dataframe object containing bibliographic references.
#'
#' @returns A dataframe object of references with a column indicating whether the article has been retracted (according to the RetractionWatch database).
#' @importFrom dplyr mutate case_when filter %>% 
#' @importFrom stringr str_remove_all str_to_lower
#' @importFrom synthesisr read_refs
#' @export
#'
#' @examples Add later.
check_retracted <- function(refs) {
  
  # Convert all 'NA's to NAs
  refs[refs == "NA"] <- NA
  
  # Ensure required columns exist
  required_cols <- c('source_type', 'author', 'year', 'title', 'journal', 
                     'volume', 'issue', 'start_page', 'end_page', 
                     'abstract', 'doi', 'publisher')
  for (col in required_cols) {
    if (is.null(refs[[col]])) refs[[col]] <- NA
  }
  
  # Subset relevant columns
  refs <- refs[required_cols]
  
  # Total records
  n_total <- nrow(refs)
  
  # Search for "retracted"
  
  # retracted<-read.csv("https://api.labs.crossref.org/data/retractionwatch?name@email.org")
  
  retracted <- read.csv("https://raw.githubusercontent.com/Mark-Eng/bibfix/refs/heads/master/data/retraction_watch.csv") 
  
  # Create clean title column for title matching
  retracted <- retracted %>% 
    mutate(clean_title = str_remove_all(str_to_lower(Title),  "[[:punct:]]"))
  
  # Find retracted articles by DOI/title
  refs <- refs |> 
    mutate(clean_title = str_remove_all(str_to_lower(title),  "[[:punct:]]")) %>%  
    mutate(isRetracted = case_when(
      doi %in% c(retracted$OriginalPaperDOI, retracted$RetractionDOI) ~ 1,
      clean_title %in% retracted$clean_title ~ 1,
      TRUE ~ 0
    ))
  
  n_retracted <- sum(refs$isRetracted)  
  
  # Return results
  output <- list(
    n_retracted = n_retracted,
    n_total = n_total
  )
  
  return(output)
}
