library("ggmap")

plot.histogram <- function(l,binwidth=7)
{
  return(
    ggplot(data=l, aes(fill=state)) +
    
    geom_histogram(aes(x=date,y=..count../sum(..count..)),binwidth=binwidth,alpha=.75,position="identity") +
    #geom_histogram(aes(x=date,y=..density..),binwidth=1,alpha=.25,position="identity") +
    geom_density(aes(x=date,y=..density..,alpha=.25,position="identity")) +
    scale_y_continuous('count / total count')
  )
}
plot.map <- function(l,deviance=0.01) 
  {
  lats=l$lat
  longs=l$long

  boundary=bounding.box(longs,lats,deviance)
  #td = as.timeDate(l$date)
  l$colors[wday(l$date) %in% c(2:6)]='Weekday'
  l$colors[wday(l$date) %in% c(1,7)]='Weekend'
#  l$colors[isHoliday(l$date)==TRUE]='Holiday'
  #print(l$colors)
  #data = cbind(l,colors)
  map=get_map(location=boundary,maptype="toner-lite",source="stamen",color="bw")
  #background=ggmap(map,extent="panel",maprange=TRUE)  
  return(
    ggmap(map,extent="panel",maprange=FALSE) +
    stat_density2d(data=l,
                 mapping=aes(x=long,y=lat,colour=colors, alpha=..level..),
                 size=1,
                 geom="density2d",
                 contour=TRUE) +  
    #scale_fill_manual(values=c("Weekday"="red", "Weekend"="green")) +
    geom_point(data=l,aes(x=long,y=lat,color=factor(colors))) +
    theme_minimal()
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
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
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
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

plot.both <- function(l,deviance=0.01) {
  multiplot(plot.map(l,deviance),plot.histogram(l))
}