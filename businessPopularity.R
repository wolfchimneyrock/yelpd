require("rmongodb")
require("lubridate")
require("magrittr")
require("data.table")
require("ggmap")
require("ggplot2")

rowVar <- function(x) {
  return(rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1))
}

lookup.review.votes <- function(state='',db=mongo,ns="yelp.reviewByBusiness")
{
  if (mongo.is.connected(db)==FALSE) db=mongo.create()
  mbj=mongo.bson.from.JSON
  pipeline=list()
  if (state !='') pipeline=c(pipeline,list(mbj(sprintf('{"$match":{"state":"%s"}}',state))))
  pipeline=c(pipeline
             ,list(mbj('{"$project":{"r":"$reviews","stars":1,"_id":0}}'))
             ,list(mbj('{"$unwind":"$r"}'))
             ,list(mbj('{"$project":{"_id":0,"rating":"$r.rating","stars":"$stars","useful":"$r.useful","cool":"$r.cool","funny":"$r.funny"}}'))
          )
  #,"funny":"$reviews.funny","cool":"$reviews.cool","useful":"$reviews.useful"
  #print(pipeline)
  res = unlist(mongo.bson.to.list(mongo.aggregation(db,ns,pipeline)))
  result=data.table(stars=res[names(res)=='result.stars'],
                    rating=res[names(res)=='result.rating'],
                    useful=res[names(res)=='result.useful'],
                    cool=res[names(res)=='result.cool'],
                    funny=res[names(res)=='result.funny'])
#  result$factor=(result$funny+result$cool+result$useful)^(1/3)
  votes=cbind(result$funny,result$cool,result$useful)
  result$factor=1/rowMeans(1/votes)
  result$var = rowSds(cbind(result$stars,result$rating))
  result$error = (result$stars-result$rating)^2
  return(result)
}
plot.review.votes<-function(v)
{
  return(
  ggplot(data=v) + 
    geom_point(aes(x=stars,
                   y=rating,
                   alpha=factor,
                   color=factor(var),
                   size=factor(error)),
               position="jitter") + 
    scale_color_manual(values=rainbow(9)) + 
    theme_minimal() +
    ylab("user supplied rating") +
    xlab("yelp computed rating") +
    theme(legend.position="none")
  )
}
lookup.review.by.stars <- function(stars=5.0,state='',db=mongo,ns="yelp.reviewByBusiness")
{
  mbj=mongo.bson.from.JSON
  pipeline=list()
  pipeline=c(pipeline
              ,list(mbj(sprintf('{"$match": {"stars":%f}}',stars)))
              ,list(mbj('{"$unwind":"reviews"}'))
              ,list(mbj('{"$group":{"_id":"$_id","date":{"$push":"$reviews.date"},"rating":{"$push":"$reviews.rating"}}}'))
  )
  res=mongo.find.all(db,ns,query=query)
  return(res)
}
cluster.business <- function(l)
{
  d = earth.dist(l)
}

bounding.box <- function(long,lat, deviance=0.01,na.rm = TRUE, ...) {
  qnt.long <- quantile(long, probs=c(deviance, 1-deviance), na.rm = na.rm, ...)
  qnt.lat <-  quantile(lat,probs=c(deviance,1-deviance),na.rm= na.rm, ...)
  return(c(c(min(qnt.long),min(qnt.lat)),c(max(qnt.long),max(qnt.lat))))
}
plot.bmap <- function(l,deviance=0.01) 
{
  lats=l$lat
  longs=l$long
  
#  l$lon=l$long
#  l$size=scale(l$count,center=FALSE)
#   l$stars=round(2*l$stars,digits=0)/2
#  print(factor(stars))
  boundary=bounding.box(longs,lats,deviance)
  map=get_map(location=boundary,maptype="roadmap",source="google",color="bw",force=FALSE)
#  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
#  sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, 8))
   dsize=0.01
   dbin=10
   dgeom="density2d"
#  return(
    ggmap(map,extent="panel",maprange=FALSE) +
      stat_density2d(data=l[l$cluster==1],aes(alpha=..level..,fill=factor(cluster),weight=size),size=dsize,bins=dbin,geom=dgeom) +
      stat_density2d(data=l[l$cluster==2],aes(alpha=..level..,fill=factor(cluster),weight=size),size=dsize,bins=dbin,geom=dgeom) +
      stat_density2d(data=l[l$cluster==3],aes(alpha=..level..,fill=factor(cluster),weight=size),size=dsize,bins=dbin,geom=dgeom) +
      stat_density2d(data=l[l$stars==4],aes(alpha=..level..,fill=factor(cluster),weight=size),size=dsize,bins=dbin,geom=dgeom) +
      stat_density2d(data=l[l$stars==5],aes(alpha=..level..,fill=factor(cluster),weight=size),size=dsize,bins=dbin,geom=dgeom) +
  
      scale_alpha_continuous("density",range=c(0.002,0.70)) +
      scale_fill_manual("density per rating",values=c("blue","cyan","green","yellowgreen","yellow","tan1","orange","orangered","red","violet")) +
#      stat_density2d(data=l[l$stars %in% 3:4,],aes(alpha=..level..,fill=..level..,weight=sqrt(count)),size=0.01,bins=24,geom="polygon") +
#      scale_fill_gradient("4 density",low="white",high="orange") +
  #      scale_fill_gradient(low="#AAAAAA",high="#AAFFAA") +
#      geom_density2d(aes(color=stars,bins=10),countour=TRUE) +   
      geom_point(data=l,aes(x=long,y=lat,color=cluster,size=sqrt(count),alpha=250/count^0.25)) +
      theme_minimal() +
      scale_size_continuous("# reviews") +
      scale_color_gradientn("rating",colours=c("blue","cyan","green","yellowgreen","yellow","tan1","orange","orangered","red","violet"))
  #    scale_color_continuous("rating",low="#EEFFEE",high="#33CC33")
#  )
}


lookup.reviewGEO.by.state.category <- function(state='',category=list(),db=mongo,ns="yelp.reviewByBusiness",limit=0,skip=0)
{
  mbj=mongo.bson.from.JSON
  if(mongo.is.connected(db)==FALSE) db<<-mongo.create()
  pipeline=list()
  if(length(category) != 0)
    pipeline=c(pipeline,list(mbj(sprintf('{"$match":{"$text":{"$search":"%s"}}}',paste(category,collapse=" ")))))
  if(state != "")
    pipeline=c(pipeline,list(mbj(sprintf('{"$match":{"state":"%s"}}',state))))
  pipeline=c(pipeline
             ,list(mbj('{"$unwind":"$reviews"}'))
#             ,list(mbj('{"$project":{ "loc":"$loc.coordinate",
#                                      "stars":"$stars",
#                                      "rating":"$reviews.rating",
#                                      "date":"$reviews.date",
#                                      "categories":"$categories"
#                                    }
#                        }'))
             ,list(mbj('{"$group":{ "_id":"$loc.coordinate",
                                    "count":{"$sum":1},
                                    "stars":{"$avg":"$reviews.rating"},
                                    "funny":{"$sum":"$reviews.funny"},
                                    "useful":{"$sum":"$reviews.useful"},
                                    "cool":{"$sum":"$reviews.cool"}
                                  }
                        }
                      ')
                  )
          )
  #print(pipeline)
  res=mongo.bson.value(mongo.aggregation(db,ns,pipeline),"result")
  res = sapply(res,function(x) {return(c(x$'_id',x$count,x$stars,x$funny,x$useful,x$cool))})
  res = as.data.table(t(res))
#  rownames(res) <- res[,1]
  setnames(res,c("lat","long","count","stars","funny","useful","cool"))
  return(res)
}