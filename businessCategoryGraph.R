require("rmongodb")
require("lubridate")
require("magrittr")

lookup.category.total <- function(category,state=NULL,parent=FALSE,db=mongo,ns="yelp.business")
  # use mongo aggregation pipeline to return a single category review count
{
  if(mongo.is.connected(db)==FALSE) db = mongo.create()
  mbj=mongo.bson.from.JSON
  pipeline=list()
  query=""
  # if we wrap the query terms with \" then it does an AND keyword search instead of OR
  #for (n in parent) query=paste0(query,' \\"',n,'\\"')
  for (n in category) query=paste0(query,' \\"',n,'\\"')
  if(length(category) != 0)
    pipeline=c(pipeline,list(mbj(sprintf('{"$match":{"$text":{"$search":"%s"}}}',query))))
  
  if(length(state) != 0)
    pipeline=c(pipeline,list(mbj(sprintf('{"$match":{"$state":"%s"}}',state))))
  
  pipeline=c(pipeline,list(mbj('{"$unwind":"$categories"}')),
             list(mbj('{"$group":{"_id":"$categories","count":{"$sum":1}}}'))
  )
  res=unlist(mongo.bson.to.list(mongo.aggregation(db,ns,pipeline)))
  result = data.frame(category=res[(names(res)=='result._id')],count=as.numeric(res[names(res)=='result.count']))
  uniques=unique(result$count)
  if(length(uniques)==0) {
    print("no results")
    return(data.frame(category=NULL,leaf=NULL,count=NULL))
  }
  uniquec=unique(result$category[result$count==max(result$count)])
  name=paste0(uniquec,collapse=".")
  #  if(length(uniquec)==1)
  #    return(data.frame(category=name,leaf=name,count=max(uniques)))
  if(parent==TRUE) {
    print(name)
    if (uniquec %in% category)
      return(data.frame(category=name,leaf=uniquec[uniquec %in% category],count=max(uniques)))
    else return(data.frame(category=name,leaf=name,count=0))
  }
  #  if(length(uniques)==1)
  #    return(data.frame(category=name,leaf=category,count=uniques))
  #  if(length(category)==1) 
  #    return (data.frame(category=name,leaf=category,count=max(uniques)))
  recurse=data.frame()
  leafs=data.frame()
  for (n in uniquec[!(uniquec %in% category)]) {
    print("Recurse up...")
    print(n)
    recurse=rbind(recurse,lookup.category.total(n))
  }
  if(length(recurse)>0)
    recurse = recurse[with(recurse,order(-count)),]
  
  if(length(uniquec)>1) {
    print("recurse across...")
    for (n in uniquec[uniquec %in% category]) {
      leafs=rbind(leafs,lookup.category.total(category=n,parent=TRUE))
    }
  } else leafs = data.frame(category=uniquec,leaf=uniquec,count=min(uniques))  
  leafs = leafs[with(leafs,order(-count)),]
  c = min(c(leafs$count,recurse$count))
  name=paste(unique(recurse$category),unique(leafs$category[leafs$count==c]),collapse="-",sep="+")
  print(name)
  
  print(c)
  result=data.frame(category=name,leaf=unique(result$category[result$count==c]),count=c)
  return(result[result$leaf %in% category,])
  #  
  #  if(length(uniques)==1)
  #    return(data.frame(category=name,count=uniques))
  #  if(length(category)==1) 
  #    return (data.frame(category=name,leaf=category,count=max(uniques)))
  #  else {
  #    print("recurse")
  #    recurse = data.frame()
  #  for (c in unique(category)) {
  #       r = result[(result$count == max(result$count)) & !(result$category %in% c),]
  #       print(r)
  #       recurse = rbind(recurse,lookup.category.total(unique(r$category)))
  #     }
  ##     recurse=recurse[with(recurse,order(-count)),]
  #     str=data.frame()
  #     for (n in unique(recurse$category)) str=rbind(str,data.frame(category=n,count=max(unique(r$count[r$category==n]))))
  #     str = str[with(str,order(-count)),]
  #     print(str)
  #     s = paste0(str$category,collapse=".")
  #     #r = result[(result$count == max(result$count)) & !(result$category %in% category),]
  #     return(data.frame(category=s,count=min(abs(recurse$count))))
  #return(result[with(result, order(-count)), ])
  #  }
}

create.category.edgelist <- function(state="",db=mongo,ns="yelp.business",limit=0,skip=0)
{
  categories = lookup.category.by.popularity(db=db,ns=ns,state=state)
  ncat = max(dim(categories))
  if(mongo.is.connected(db)==FALSE) db = mongo.create()
  if(state=="") query='{}'
  else query=sprintf('{"state":"%s"}',state)
  fields='{"_id":0,"categories":1}'
  allcat=data.table()
  allcat=mongo.find.all(db,ns,query=query,fields=fields,skip=skip,limit=limit,data.frame=FALSE)
  #  allcat <- allcat[sample(nrow(allcat)),]
  total=length(allcat)
  print(total)
  allcat=unlist(allcat,recursive=FALSE)
  #print(allcat)
  edgelist=data.table()
  expand=data.table()
  for (n in allcat) {
    #print(n)
    expand=expand.grid(as.character(n),as.character(n))
    
    expand=expand[as.character(expand$Var1)!=as.character(expand$Var2),]
    edgelist=rbindlist(list(edgelist,expand))
    #print(expand)
  }
  edgelist=edgelist[sample(nrow(edgelist)),]
  #print(dim(edgelist))
  #print(edgelist)
  print("creating graph...")
  g=graph.edgelist(as.matrix(edgelist),directed=FALSE)
  node.size=setNames(categories$count,categories$category)
  #print(node.size)
  for (n in V(g)$name) {
    #print(n)
    V(g)[n]$size=node.size[V(g)[n]$name]
  }
  #V(g)$size = scale(V(g)$size,center=FALSE)
  return(g)
}

lookup.category.by.popularity <- function(db=mongo,ns="yelp.business",parent=list(),state="")
  # use mongo aggregation pipeline to return all review counts by category
  # sort afterwards since i can't seem to sort in it mongodb
{
  if(mongo.is.connected(db)==FALSE) db = mongo.create()
  mbj = mongo.bson.from.JSON
  pipeline=list()
  # note - the text search $match must be the first entry in the pipeline
  if(length(parent) != 0)
    pipeline=c(pipeline,list(mbj(sprintf('{"$match":{"$text":{"$search":"%s"}}}',paste(parent,collapse=" ")))))
  if(state != "")
    pipeline=c(pipeline,list(mbj(sprintf('{"$match":{"$state":"%s"}}',state))))
  pipeline=c(pipeline,
             list(mbj('{"$unwind":"$categories"}')),
             list(mbj('{"$group":{"_id":"$categories","count":{"$sum":1}}}'))
  )
  res=unlist(mongo.bson.to.list(mongo.aggregation(db,ns,pipeline)))
  result = data.frame(category=as.character(res[names(res)=="result._id"]),count=as.numeric(res[names(res)=="result.count"]))
  return(result[with(result, order(-count,category)), ])
}

lookup.reviews.by.state.category <- function(db = mongo, ns = "yelp.reviewByBusiness",state="NV",category)
{
  if(mongo.is.connected(db)==FALSE) db=mongo.create()
  rpipeline=list(
    mongo.bson.from.JSON(sprintf('{"$match": {"state":"%s","$text":{"$search":"%s"}}}',state,category)),
    mongo.bson.from.JSON('{"$unwind":"$reviews"}'),
    mongo.bson.from.JSON('{"$project":{"_id":"$reviews.date"}}'),
    mongo.bson.from.JSON('{"$group":{"_id":"$_id","count":{"$sum":1}}}'),
    mongo.bson.from.JSON('{"$sort":{"_id":1}}')
  )
  print(rpipeline)
  tpipeline=list(
    mongo.bson.from.JSON(sprintf('{"$match": {"state":"%s","$text":{"$search":"%s"}}}',state,category)),
    mongo.bson.from.JSON('{"$unwind":"$tips"}'),
    mongo.bson.from.JSON('{"$project":{"_id":"$tips.date"}}'),
    mongo.bson.from.JSON('{"$group":{"_id":"$_id","count":{"$sum":1}}}'),
    mongo.bson.from.JSON('{"$sort":{"_id":1}}')
  )
  revs = unlist(mongo.bson.to.list(mongo.aggregation(db,ns,rpipeline)))
  tips = unlist(mongo.bson.to.list(mongo.aggregation(db,ns,tpipeline)))
  result = data.frame()
  r = data.frame(date=revs[names(revs)=="result._id"],rcount=as.numeric(revs[names(revs)=="result.count"]))
  t = data.frame(date=tips[names(tips)=="result._id"],tcount=as.numeric(tips[names(tips)=="result.count"]))
  result = merge(r,t)
  result[is.na(result)] <- 0
  result$date=as.Date(result$date,origin="1970-01-01")
  return(result)
}
