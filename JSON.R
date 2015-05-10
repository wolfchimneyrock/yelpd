#install.packages("jsonlite","plyr")
library("jsonlite")
library("plyr")
readFromJSON <- function(fname)
{
  return(fromJSON(fname))
}
