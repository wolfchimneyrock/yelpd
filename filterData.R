#install.packages("foreach")
library("foreach")
library("rmongodb")
filter.business <- function(data,states=NULL,cities=NULL, categories=NULL)
{
  
  if (!is.null(states)) {
    states.all = unique(data$state)
    state.keep = intersect(states.all,states)
    data = data[(data$state) %in% list(state.keep),]
  }
  
  if (!is.null(cities)) {
    cities.all = unique(data$city)
    city.keep = intersect(cities.all,cities)
    data = data[(data$city) %in% list(city.keep),]
  }
  
  if (!is.null(categories)) {
    cat.all = unique(data$categories)
    
    cat.keep = foreach(c=cat.all) %do% { if (any(categories %in% unlist(c))) c } 
    cat.keep = cat.keep[-(which(sapply(cat.keep,is.null),arr.ind=TRUE))]
    
    data = data[data$categories %in% cat.keep,]
  }
  return(data)
}

filter.reviewtext <- function(reviews)
{
  #nwords <- unlist(foreach(r=reviews$text) %do% )
}
filter.userfriends <- function(users)
{
  nfriends <- unlist(foreach(u=users$friends) %do% {length(unlist(u))})
  users$friends <- NULL
  return(cbind(users,nfriends))
}
combine.userreviewdata <- function(review,user,business)
{
   stars = business$stars
   
}