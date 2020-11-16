
# Explanation of function
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
  n_total = nrows(pts)
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
    dist_from_est <- head(dist_from_est, local_size)
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
              weights=weights, data = local_pts)

    # use local polynomial to compute value of regression at point of estimation
    reg_val = predict(fit, newdata=data.frame(x=pts$x[i]))
    reg_vals <- cbind(reg_vals, reg_val)
    SSE = sum(fitted(fit) - mean(pts$y))^2

    
    print(summary(fit))
    print(local_pts)
    print(reg_val)
    break
  }
  
  res = list(span = span,
             degree = degree,
             N_total = n_total,
             Win_total = n_windows,
             n_points = n_local,
             SSE = SSE,
             loessplot = 0)
  return(res)
}

#Your function should return a named list containing the following:
#  span: proportion of data used in each window (controls the bandwidth)
#degree: degree of polynomial
#N_total: total number of points in the data set
#Win_total: total number of windows
#n_points: number of points in each window in a vector
#SSE: Error Sum of Squares (Tells us how good of a fit we had).
#loessplot: An object containing the ggplot so that we can see the plot later.
#We want this even if show.plot = FALSE
#Note: you are NOT allowed to simply use stat_smooth() or geom_smooth() to have it automatically do LOESS.
#You should use geom_line() or similar to plot your final the LOESS curve.
# Make sure you can access the objects properly using the $ notation.

# set wd and get the sample data
setwd('~/School/Fall\ 2020/CMDA4654/Projects/CMDA-4654-Project1/')
data <- read.csv('./example.csv')

res <- myloess(data$x, data$y, span=0.33, degree = 1, show.plot=TRUE)
