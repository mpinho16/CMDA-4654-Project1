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
  n_local = ceiling(nrow(pts)*span)
  n_total = nrow(pts)
  n_windows = 0
  reg_vals = vector()
  SSE = 0
  
  # loop through every point in the data
  for (i in 1:n_total) {

    # get the distances from x[i] to every other x
    dist_from_est <- matrix(NA, nrow = n_total, ncol = 2)
    for(j in 1:n_total) {
      dist_from_est[j,] <- c(abs(pts$x[i] - pts$x[j]), j)
    }
    
    # sort dist matrix & only keep local pts
    dist_from_est <- dist_from_est[order(dist_from_est[,1]),]
    dist_from_est <- head(dist_from_est, n_local)
    local_pts <- data.frame(x=pts$x[dist_from_est[,2]], y=pts$y[dist_from_est[,2]])
    local_pts <- cbind(local_pts, dist=dist_from_est[,1])
    
    # scale the distances
    scale_factor <- 1/tail(local_pts$dist, n=1)
    local_pts <- cbind(local_pts, scaled = local_pts$dist*scale_factor)
    
    # determine weights for points selected in previous part
    weights = vector()
    for(d in local_pts$scaled){
      if(abs(d) < 1) {
        wd = (1-abs(d)^3)^3
      } else {
        wd = 0
      }
      weights <- c(weights, wd)
    }
    local_pts <- cbind(local_pts, weights)

    # perform weighted least squares on local data set
    fit <- lm(y ~ poly(x, degree, raw=TRUE),
              weights = weights, data = local_pts)

    # use local polynomial to compute value of regression at point of estimation
    reg_val = predict(fit, newdata=data.frame(x=pts$x[i]))
    reg_vals <- cbind(reg_vals, reg_val)
    n_windows = n_windows + 1
  }
  
  # create a plot with the model
  p = ggplot(pts, aes(x, y)) + theme_bw() + 
    geom_point() + geom_line(aes(x, y=reg_vals)) +
    ggtitle('LOESS Regression')
  
  if(show.plot) {
    print(p)
  }
  
  SSE = sum((pts$y - reg_vals)^2)
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