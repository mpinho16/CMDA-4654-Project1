tukey_triweight <- function(u) {
  if (abs(u) <= 1) {
    return((1-abs(u)^3)^3)
  }
  return(0)
}

## @knitr myloess

# myloess computes the loess function of a predictor variable
# x - the predictor variable
# y - the explanatory set
# span - determines size of local data neighborhood
#        should be between (0,1)
# degree - determines degree of polynomial used
#          should be 1 or 2 for best results
# show.plot - true to show the plot, false to not show it
myloess <- function(x, y, span = 0.5, degree = 1, show.plot = TRUE) {
  
  # size of local data neighborhoods used is span * n
  pts = data.frame(x = x, y = y)
  pts = pts[order(x),] #orders dataframe by x
  
  n_local = ceiling(nrow(pts)*span) #number of points in a window
  n_total = nrow(pts) # total number of points
  n_windows = 0
  fitted_vals = c()
  
  # loop through every point in the data
  for (i in 1:n_total) {
    current_pt = pts[i,1]
    
    # get n_local points with closest x coordinates
    local_pts = pts
    local_pts$dist = abs(pts$x-current_pt)
    local_pts <- head(local_pts[order(local_pts$dist),], n_local)
    
    # determine Tukey tri-weights for each point
    maxdist = max(local_pts$dist)
    local_pts$weights <- sapply(local_pts$dist/maxdist,tukey_triweight)

    # perform weighted least squares on local data set
    fit <- lm(y ~ poly(x, degree, raw=TRUE),
              weights = weights, data = local_pts)

    # use local polynomial to compute value of regression at point of estimation
    fitted_vals <- c(fitted_vals, 
                     predict(fit, newdata = data.frame(x=current_pt)))
    
    n_windows = n_windows + 1
  }
  
  # create a plot with the model
  p = ggplot(pts, aes(x, y)) + theme_bw() + 
    geom_point() + geom_line(aes(x, y=fitted_vals)) +
    ggtitle('LOESS Regression')
  
  if(show.plot) {
    print(p)
  }
  
  SSE = sum((pts$y - fitted_vals)^2)
  # list of objects to return
  res = list(span = span,
             degree = degree,
             N_total = n_total,
             Win_total = n_windows,
             n_points = n_local,
             SSE = SSE,
             loessplot = p)
  return(res)
}
