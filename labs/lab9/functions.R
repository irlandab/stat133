#' @title Range
#' @description Computes overall range: max - min
#' @param x numeric vector
#' @return computed range
stat_range <- function(x) {
  max(x) - min(x)
}

#' @title Centers measures
#' @description Computes measures of center such as Median and Mean
#' @param x numeric vector
#' @return a numeric vector with median and mean
stat_centers <- function(x) {
  y <- c(0, 0)
  y[1] <- median(x)
  y[2] <- mean(x)
  names(y) <- c("Median", "Mean")
  return(y)
}

#' @title Spread measures
#' @description Computes measures of spread: range, IQR, and SD
#' @param x numeric vector
#' @return vector with range, iqr, and stdev
stat_spreads <- function(x) {
  y <- c('range' = stat_range(x),
         'iqr' = IQR(x),
         'stdev' = sd(x))
  return(y)
}