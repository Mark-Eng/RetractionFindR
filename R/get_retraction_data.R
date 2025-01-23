# Sources RetractionWatch data from Crossref GitLab as recommended (as of 2024-12-19) at: https://www.crossref.org/documentation/retrieve-metadata/retraction-watch/  

# Object for url
url <- "https://gitlab.com/crossref/retraction-watch-data/-/raw/main/retraction_watch.csv?ref_type=heads"

# Read csv from url
retwatch_db<-read.csv(url)

# Update the csv in the 'data' directory
write.csv(retwatch_db,  file = 'data/retraction_watch.csv')

# Add a file describing when the data were last updated
cat("Retraction Watch data last retrieved on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), file = "data/latest_update.txt")