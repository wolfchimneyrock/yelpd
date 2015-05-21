library("ggmap")
library("fpc")
plot.fft <- function(l)
{
  library(plyr)
  mytimeseries=decimal_date(l$date)*365
  
  ## you can plug in any array here for mytimeseries
  ## each row is a timeseries
  ## for series by column, change the margin from 1 to 2, below
#  logspec = adply( mytimeseries, 2, function(x) {
#    ## change spec.pgram parameters as per goals
#    ret = spectrum(x, taper=0, demean=T, pad=0, fast=T, plot=F)
#    return( data.frame(freq=ret$freq, spec=ret$spec))
#  })
  logsp = spectrum(mytimeseries,taper=0,demean=T,pad=0,fast=T,plot=F) 
  logspec = data.frame(freq=logsp$freq,spec=logsp$spec)
  ## boxplot stats at each unique frequency
  logspec.bw = ddply(logspec, 'freq', function(x) {
    ret = boxplot.stats(log10(x$spec));
    ## sometimes $out is empty, bind NA to keep dimensions correct
    return(data.frame(t(10^ret$stats), out=c(NA,10^ret$out)))
  })
  
  ## plot -- outliers are dots
  ## be careful with spacing of hard returns -- ggplot has odd rules
  ## as close to default "spectrum" plot as possible
  ggplot(logspec.bw, aes(x=1/(freq)))  +
    geom_ribbon(aes(ymin=X1, ymax=X5), alpha=0.35, fill='green')  +
    geom_ribbon(aes(ymin=X2, ymax=X4), alpha=0.5, fill='blue') +
    geom_line(aes(y=X3))  +
#    geom_point(aes(y=out), color='darkred') +
    scale_x_continuous(name='Period')  +
    scale_y_continuous(name='Power', trans='log10')
}
plot.histogram <- function(l,binwidth=14,palette=NULL)
{
  xpos=min(l$date)
  ypos=1
  binwidth=length(l)
  l[is.na(l)] <- 0
  return(
    ggplot(data=l) +
    geom_density(aes(x=date,y=..scaled..,fill=factor(cluster)),color="gray",alpha=.125,position="dodge") +  
    geom_density(aes(x=date,y=-..scaled..,fill=factor(state)),color="gray",alpha=.125,position="dodge") +      
    geom_histogram(data=l,aes(x=date,y=..ncount..,fill=factor(cluster)),binwidth=binwidth,alpha=1,position="identity") +
    geom_histogram(data=l,aes(x=date,y=-..ncount..,fill=factor(state)),binwidth=binwidth,alpha=1,position="identity") +
    geom_rug(aes(x=date,y=0,color=state),position="identity",sides="b") +
    scale_fill_manual("clusters\nstates",values=palette) +
    scale_color_manual("clusters\nstates",values=palette) +
      #    scale_colour_discrete(drop=TRUE,limits) +
    geom_rug(aes(x=date,y=0,color=factor(cluster)),position="dodge",sides="t") +
    #scale_color_discrete() +
    #geom_line(aes(x=date,y=diff),color="gray") +
    #geom_line(aes(x=date,y=rolling),color="blue") +
      annotate("text",x=xpos,y=ypos,label="cluster",size=3,hjust=0) +
      annotate("text",x=xpos,y=-ypos,label="state",size=3,vjust=0,hjust=0) +
    theme(legend.position="none",panel.background=element_rect(fill="white")) +
    ylab("normalized count by state / cluster") +
#    scale_y_sqrt('normalized count + density') +
    theme_minimal()
    )
}
plot.timesince <- function(l,palette=NULL)
{
  l$date=decimal_date(l$date)
  #ql = as.data.frame(Quantile.loess(Y=l$diff,X=l$date,the.quant=0.99,window.size=7,window.alignment="right"))
  xpos=min(l$date)
  ypos=max(l$diff)
  sd=sqrt(sd(l$diff))
  gaps=data.frame()
# for (n in l$diff[l$diff>sd]) {
#    gaps=rbind(gaps,c(start=l$date[n-1],end=l$date[n])) 
#  }
#  colnames(gaps)=c('start','end')
  gaps = as.data.frame(cbind(start=l$date[l$diff>sd],end=l$date[c(FALSE,l$diff>sd)]))
  gaps2 = gaps[gaps$end-gaps$start>0.018,]
  if(nrow(gaps2)!=0) gaps=gaps2
  print(gaps)
  return(
   ggplot(data=l) +
     geom_rect(data=gaps,aes(xmin=start,xmax=end,ymin=-Inf,ymax=Inf),fill='pink',alpha=0.3) +
     geom_line(aes(x=date,y=diff),color="gray") +
     geom_line(aes(x=date,y=rolling),color="blue") +
     stat_smooth(aes(x=date,y=diff),color="red") +
#     geom_line(data=ql,aes(x=x,y=y),color="red") +
#     geom_line(data=ql,aes(x=x,y=y.loess),color="red") +
     geom_rug(data=l,aes(x=date,color=factor(cluster)),sides="b") +
     geom_rug(data=l,aes(x=date,color=factor(state)),sides="t") +
     annotate("text",x=xpos,y=ypos,label="state",size=3,hjust=0) +
     annotate("text",x=xpos,y=-10,label="cluster",size=3,vjust=0,hjust=0) +
     annotate("text",x=xpos,y=0.90*ypos,label="raw data",size=3,color="gray",hjust=0) +
     annotate("text",x=xpos,y=0.83*ypos,label="7 day rolling average",size=3,color="blue",hjust=0) +
     annotate("text",x=xpos,y=0.76*ypos,label="quantile smoothed",size=3,color="red",hjust=0) +
     annotate("text",x=xpos,y=0.69*ypos,label="duration of no reviews",size=3,color="pink",hjust=0) +
#     scale_colour_discrete(drop=TRUE,limits) +
     scale_color_manual("cluster\nstate",values=palette) +
     theme(legend.position="none",panel.background=element_rect(fill="white")) +
     ylab('days since last review') +
     theme_minimal()
     #scale_y_sqrt('days since last review')
   )
}
plot.dayofweek <- function(l,labels=TRUE,palette=NULL)
{
  l$day=wday(l$date,label=labels)
  xpos=min(l$date)
  ypos=max(l$diff)
  return(
    ggplot(data=l,aes(x=day))
      + scale_fill_manual("clusters\nstates",values=palette)
      + scale_color_manual("clusters\nstates",values=palette)
      + geom_bar(aes(stat="bin",y=-sqrt(..count..),fill=state))
      + geom_bar(aes(stat="bin",y=sqrt(..count..),fill=factor(cluster)))
#      scale_fill_discrete(drop=TRUE,limits)
      + theme(panel.background=element_rect(fill="white"))
      + annotate("text",x=0,y=-1,label="state",size=3,hjust=0)
      + annotate("text",x=0,y=0.5,label="cluster",size=3,vjust=0,hjust=0)
      + geom_hline(aes(y=0),color="white",linetype="solid",size=3)
      + geom_hline(aes(y=0),color="black",linetype="solid",size=0.25)
      + scale_y_continuous('reviews per weekday',labels=abs)
  )
}
plot.map <- function(l,deviance=0.05,palette=NULL) 
  {
  lats=l$lat
  longs=l$long

  boundary=bounding.box(longs,lats,deviance)
  #td = as.timeDate(l$date)
  l$day[wday(l$date) %in% c(2:6)]='Weekday'
  l$day[wday(l$date) %in% c(1,7)]='Weekend'
#  l$colors[isHoliday(l$date)==TRUE]='Holiday'
  #print(l$colors)
  #data = cbind(l,colors)
  map=get_map(location=boundary,maptype="toner-lite",source="stamen",color="bw",force=FALSE)
  #background=ggmap(map,extent="panel",maprange=TRUE)  
  return(
    ggmap(map,extent="panel",maprange=FALSE) +
#    stat_density2d(data=l,
#                 mapping=aes(x=long,y=lat,
#                 color=factor(cluster),alpha=..level..),
#                 size=1,
#                 geom="density2d",
#                 contour=TRUE) +  
    #scale_fill_manual(values=c("Weekday"="red", "Weekend"="green")) +
    geom_point(data=l,aes(x=long,y=lat,color=factor(cluster), shape=factor(day))) +
    scale_color_manual("Clusters",values=palette) +
    theme_minimal()
    #    theme(legend.position="none")
    #ggplot(data=l,aes(x=long,y=lat,color=date))
  )
}

bounding.box <- function(long,lat, deviance=0.01,na.rm = TRUE, ...) {
  qnt.long <- quantile(long, probs=c(deviance, 1-deviance), na.rm = na.rm, ...)
  qnt.lat <-  quantile(lat,probs=c(deviance,1-deviance),na.rm= na.rm, ...)
  return(c(c(min(qnt.long),min(qnt.lat)),c(max(qnt.long),max(qnt.lat))))
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL,title="unnamed") {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    #grid.text(title,gp=gpar(fontsize=9),x=0,y=0,just=c("left","top"))
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
  grid.text(title,gp=gpar(fontsize=12))
}
plot.top.users.to.pdf <- function(nrows=1,skip=0,onefile=TRUE,filename="test",db=mongo,ns="yelp.userLocation")
{
  library(grDevices)
  source("lookupData.R")
  if(mongo.is.connected(mongo)==FALSE) mongo<<-mongo.create()
  palette=lookup.states(nrows=nrows,skip=skip)
  pdf(file=ifelse(onefile, paste0(filename,'.pdf'),paste0(filename,'%03d.pdf')), width=17,height=11,onefile=TRUE)
  for (n in 1:nrows) {
    l=lookup.top.users.location.history(nrows=1,skip=skip)
    plot.all(l,title=unique(l$user_id),palette=palette)    
    skip = skip + 1
  }
  dev.off()
}
plot.all <- function(l,deviance=0.075,title="unnamed",palette=NULL) {
  multiplot(plot.map(l,deviance,palette=palette),plot.dayofweek(l,palette=palette),plot.histogram(l,palette=palette),plot.timesince(l,palette=palette),cols=2,title=title)
}