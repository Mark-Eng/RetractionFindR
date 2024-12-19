# Sources RetractionWatch data from Crossref GitLab as recommended (as of 2024-12-19) at: https://www.crossref.org/documentation/retrieve-metadata/retraction-watch/  

#object for url
url <- "https://gitlab.com/crossref/retraction-watch-data/-/raw/main/retraction_watch.csv?ref_type=heads"

#read csv from url
retwatch_db<-read.csv(url)

#update the csv in the 'data' directory
write.csv(retwatch_db,  file = 'data/retraction_watch.csv')