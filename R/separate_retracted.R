library(dplyr)


#' separate_retracted
#' @description Separates a list of references into retracted and non-retracted articles.
#' @param df A dataframe object of bibliographic data, including column with a binary indicator of whether the article has been retracted (according to the RetractionWatch database).
#'
#' @returns A list of dataframes, one containing only retracted articles and one containing only non-retracted ones.
#' @export
#'
#' @examples Add later
separate_retracted <- function(df) {
  retracted_articles <- df %>% 
    filter(isRetracted == 1)
  
  non_retracted <- df %>% 
    filter(isRetracted == 0)
  
  list(retracted_articles, non_retracted)
}
