library("rmongodb")
library("lubridate")
library("magrittr")
library("zoo")
library("bigmemory")


generate.user.history <- function(history, count=1)
{
  hist = mongo.bson.to.list(history)
  iter = mongo.bson.iterator.create(history)
  loc = vector()
  mat = matrix(nrow=0,ncol=2)
  i = 0
  lastdate=0
  while ((1<count)&&(mongo.bson.iterator.next(iter))) {
    val = mongo.bson.iterator.value(iter)
    date = decimal_date(mongo.bson.value(val,"date"))
    loc = mongo.sbon.value(val,"loc.coordinate")
    #rownames(loc) <- date
    mat = rbind(mat,loc)
    i = i + 1
    lastdate=date
  }

  return(mat)
}
ma <- function(x,n=5,sides=1){filter(x,rep(1/n,n), sides=sides)}

lookup.top.users.location.history <- function(skip=0,nrows=10,db = mongo, ns = "yelp.userLocation",window=7)
{ 
  library(fpc)
  library(cluster)
  query <- '{}'
  fields <- '{}'
  sort <- '{"count":-1}'
#  history = mongo.bson.empty()
  cursor = mongo.find(db,ns,query=query,sort=sort,fields=fields,skip=skip,limit=nrows)
#  rmat = matrix(nrow=0,ncol=10)
#  coord = vector()
  ## iterate over the cursor
  lat = vector()
  long = vector()
  l = data.frame()
  locs = data.frame()
  while ((mongo.cursor.next(cursor))) {
    value = mongo.cursor.value(cursor)
    count = mongo.bson.value(value,"count")
    user_id = rep(mongo.bson.value(value,"_id"),count)
    date=as.character(mongo.bson.value(value,"dates"))
    #date = as.Date(as.POSIXct(mongo.bson.value(value,"dates")))
    #d = mongo.bson.value(value,"dates")
    #date=as.Date(as.POSIXct(d, origin="1970-01-01"))
    #names(dimnames) = c('date','lat','long')
    lat = mongo.bson.value(value,"lat")
    long = mongo.bson.value(value,"long")
    state = mongo.bson.value(value,"state")
    #print(dim(lats))
    #print(dim(longs))
    locs = cbind(user_id,date,lat,long,state)
    #rownames(locs) = rep(user_id,count)
    
    print(dim(locs))
    #locations = as.matrix(locs,nrow=count,ncol=2,dimnames=dimnames)
    l=rbind(l,locs)
  }
  l$lat=as.numeric(as.character(l$lat))
  l$long=as.numeric(as.character(l$long))
  l$date=as.Date(as.POSIXct(l$date,origin="1970-01-01"))
  l$diff=c(0,diff(l$date))
#  l$rolling = c(rep(0,window-1),rollmean(l$diff,window))
  l$rolling=ma(l$diff)
  d=data.frame(scale(l$long),scale(l$lat),scale(decimal_date(l$date)))
  print(skip)
  print("clustering...")
  pamk.best <- pamk(d,usepam=T)
  p=pam(d, pamk.best$nc)
  l$cluster=p$clustering
  return (l)
  
}
lookup.states <- function(skip=0,nrows=1,db=mongo,ns="yelp.userLocation") 
{
  query='{}'
  fields='{"_id":0,"state":1}'
  sort='{"count":-1}'
  states=unique(unlist(mongo.find.all(db,ns,query=query,skip=skip,limit=nrows,fields=fields,sort=sort,data.frame=FALSE)))
#  s=1:length(states)
#  names(s)=states
  states.col=c(rainbow(length(states)),rainbow(10))
  names(states.col)=c(states,1:10)
  return (states.col)
}
lookup.review.dates <- function(skip=0,limit=0,db=mongo, ns="yelp.review") {
  query = sprintf('{}')
  fields = '{"_id":0,"date":1}'
  sort = '{"date":1}'
  return(mongo.find.all(db,ns,query=query,skip=skip,limit=limit,fields=fields,sort=sort,data.frame=FALSE))
}

lookup.user.info <- function(user_id,db = mongo, ns = "yelp.user")
{
  query = sprintf('{"user_id":"%s"}',user_id)
  fields='{"yelping_since":1,"votes":1,"review_count":1,"fans":1,"average_stars":1,"compliments":1,"elite":1}'
  tmp = mongo.find.one(db,ns,query = query, fields=fields)
  return(unlist(mongo.bson.to.list(tmp),recursive=FALSE))
}
lookup.business.stars <- function(business_id,db = mongo, ns = "yelp.business")
{
  query = sprintf('{"business_id":"%s"}',business_id)
  tmp = mongo.find.one(db,ns,query = query, fields='{"stars":1}')
  return(as.numeric(mongo.bson.value(tmp,"stars")))
}
lookup.reviews.by.user <- function(user_id, fields='{"_id":0}', db = mongo, ns = "yelp.review")
{
  query = sprintf('{"user_id":"%s"}',user_id)
  tmp = mongo.find.all(db,ns,query = query, fields = fields)
  return(tmp)
}
lookup.tips.by.business <- function(business_id,fields='{"_id":0,"checkin_info":1}',db = mongo, ns = "yelp.checkin")
{
  query = sprintf('{"business_id":"%s"}',business_id)
  tmp = mongo.find.all(db,ns,query=query,fields=fields)
  return(tmp)
}
lookup.reviews.by.business <- function(business_id,fields='{"_id":0}', db = mongo,ns = "yelp.review")
{
  query = sprintf('{"business_id":"%s"}',business_id)
  tmp = mongo.find.all(db,ns,query = query, fields=fields)
  return(tmp)
}

lookup.review.text <- function(review_id,db = mongo, ns = "yelp.review",fields='{"_id":0,"text":1}')
{
  query = sprintf('{"review_id":"%s"}',review_id)
  tmp = mongo.find.one(db,ns,query = query, fields=fields)
  return(mongo.bson.value(tmp,"text"))
}
lookup.review.notext <- function(review_id,db = mongo, ns = "yelp.review",fields='{"_id":0,"text":0}')
{
  query = sprintf('{"review_id":"%s"}',review_id)
  tmp = mongo.find.one(db,ns,query = query, fields=fields)
  return(mongo.bson.to.list(tmp))
}
