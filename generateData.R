library("lubridate")
library("magrittr")
library("dplyr")
library("foreach")

source("lookupData.R")
generate.categories <- function(db=mongo,ns="yelp.business",state="",parent="")
{
  categories=lookup.category.by.popularity(db=db,ns=ns,state=state,parent=parent)
  total = lookup.category.total(parent)
  sum=0
  foreach(c=categories$category) %do% lookup.category.total(c,parent=parent) when (sum<total)
#  categories %$% .$category[.$category %in% parent] %>% as.character %>% print
  
}
generate.user.review.curve <- function(user)
{
  user = lookup.user.info(user_id)
  reviews = lookup.reviews.by.user(user$user_id, fields = '{"date":1,"votes":1}')
  date.start = user$yelping_since
  
}
generate.review.row <- function(review)
{
  row = vector()
  truestars = lookup.business.stars(mongo.bson.value(review,"business_id"))
  user = lookup.user.info(mongo.bson.value(review,"user_id"))
  #  nfriends = lookup.user.nfriends(review$user_id)
  stars = mongo.bson.value(review,"stars")
  label = abs(truestars-stars)
  rdate = ymd(mongo.bson.value(review,"date"))
  udate = ymd(user$yelping_since,truncated=1)
  age = as.numeric(rdate - udate)
  ureviews = user$review_count
  funny = mongo.bson.value(review,"votes.funny")
  useful = mongo.bson.value(review,"votes.useful")  
  cool = mongo.bson.value(review,"votes.cool")
  fans = user$fans
  avg = user$average_stars
  compliments = sum(unlist(user$compliments),na.rm=TRUE)
  
  row = (cbind(label,stars,avg,age,ureviews,funny,useful,cool,fans, compliments))
}
generate.review.matrix <- function(nrows=10,db = mongo, ns = "yelp.review")
{
  ## create the counter
  i = 1
  cursor = mongo.find(db,ns,fields='{"text":0}')
  rmat = matrix(nrow=0,ncol=10)
  ## iterate over the cursor
  while ((i<=nrows)&(mongo.cursor.next(cursor))) {
    # iterate and grab the next record
    value = mongo.cursor.value(cursor)
    #print(value)
    #tmp = mongo.bson.to.list(value,simplify=TRUE)
    row = generate.review.row(value)
    rownames(row)<-mongo.bson.value(value,"business_id")
    rmat = rbind(rmat,row)
    #cat('.')
    i = i + 1
  }
  
  return(rmat)
}