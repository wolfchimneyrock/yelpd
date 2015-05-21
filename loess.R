# This code relies on the rollapply function from the "zoo" package.  My thanks goes to Achim Zeileis and Gabor Grothendieck for their work on the package.
Quantile.loess<- function(Y, X = NULL,
                          number.of.splits = NULL,
                          window.size = 20,
                          percent.of.overlap.between.two.windows = NULL,
                          the.distance.between.each.window = NULL,
                          the.quant = .95,
                          window.alignment = c("center"),
                          window.function = function(x) {quantile(x, the.quant)},
                          # If you wish to use this with a running average instead of a running quantile, you could simply use:
                          # window.function = mean,
                          ...)
{
  # input: Y and X, and smothing parameters
  # output: new y and x
  
  # Extra parameter "..." goes to the loess
  
  # window.size ==  the number of observation in the window (not the window length!)
  
  # "number.of.splits" will override "window.size"
  # let's compute the window.size:
  if(!is.null(number.of.splits)) {window.size <- ceiling(length(Y)/number.of.splits)}
  
  # If the.distance.between.each.window is not specified, let's make the distances fully distinct
  if(is.null(the.distance.between.each.window)) {the.distance.between.each.window <- window.size}
  
  # If percent.of.overlap.between.windows is not null, it will override the.distance.between.each.window
  if(!is.null(percent.of.overlap.between.two.windows))
  {
    the.distance.between.each.window <- window.size * (1-percent.of.overlap.between.two.windows)
  }
  
  
  # loading zoo
  if(!require(zoo))
  {
    print("zoo is not installed - please install it.")
    install.packages("zoo")
  }
  
  
  if(is.null(X)) {X <- index(Y)} # if we don't have any X, then Y must be ordered, in which case, we can use the indexes of Y as X.
  
  # creating our new X and Y
  zoo.Y <- zoo(x = Y, order.by = X)
  #zoo.X <- attributes(zoo.Y)$index
  
  new.Y <- rollapply(zoo.Y, width = window.size,
                     FUN = window.function,
                     by = the.distance.between.each.window,
                     align = window.alignment)
  new.X <- attributes(new.Y)$index
  new.Y.loess <- loess(new.Y~new.X, family = "sym",...)$fitted
  
  return(list(y = new.Y, x = new.X, y.loess = new.Y.loess))
}