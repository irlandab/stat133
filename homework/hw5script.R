check_sides <- function(x) {
  if (length(x) != 6) {
    stop("'sides' must be a vector of length 6")
  }
}

check_prob <- function(x) {
  if (sum(x) != 1) {
    stop("elements in 'prob' must add up to 1")
  }
}

die <- function(sides = 1:6, prob = rep(1/6, 6)) {
  check_sides(sides)
  check_prob(prob)
  df <- data.frame(sides,prob)
  cat("object \"die\"\n\n")
  print.data.frame(df)
}

check_times <- function(times) {
  if (times < 1) {
    stop("'times' must be greater than 0")
  }
  else if (times %% 1 != 0) {
    stop("'times' must be a whole number")
  }
  else if (!is.double(times)){
    stop("'times' must be an integer")
  }
}

roll <- function(die, times = 1) {
  check_times(times)
  cat("object \"roll\"\n\n")
  output <- list(rolls = sample(die$sides, size = times, replace = TRUE),
                 sides = die$sides,
                 prob = die$prob,
                 total = times)
  return(output[1])
}