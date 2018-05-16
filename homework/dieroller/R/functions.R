#' @title check_sides
#' @description checks the validity of the argument sides in the function die
#' @param x a vector
#' @return an error message if the sides of the die are invalid
check_sides <- function(x) {
  if (length(x) != 6) {
    stop("'sides' must be a vector of length 6")
  }
}

#' @title check_prob
#' @description checks the validity of the argument prob in the function die
#' @param x a vector
#' @return an error message if the probabilities of the die are invalid
check_prob <- function(x) {
  if (sum(x) != 1) {
    stop("elements in 'prob' must add up to 1")
  }
}

#' @title die
#' @description craetes a die
#' @param sides a vector
#' @param prob a vector
#' @return object of class "die" which is a list
die <- function(sides = 1:6, prob = rep(1/6,6)) {
  check_sides(sides)
  check_prob(prob)
  object <- list(sides=sides,prob=prob)
  class(object) <- "die"
  return(object)
}

#' @title print.die
#' @description a "print" method for "die" objects
#' @param die object of class "die" which is a list
#' @return a tabular display of the die's sides and associated probabilities
print.die <- function(die) {
  cat('object "die"\n\n')
  print.data.frame(data.frame(sides = die$sides, prob = die$prob))
  invisible(die)
}

#' @title check_times
#' @description checks the validity of the argument times in the function roll
#' @param times a scalar
#' @return an error message if the number of times is invalid
check_times <- function(x) {
  if (!is.double(x)) {
    stop("times must be a number")
  } else if ((x < 1) | (x %% 1 != 0)) {
    stop("times must be an integer greater than 0")
  }
}

#' @title roll
#' @description simulates the rolling of a die
#' @param die object of class "die" which is a list
#' @param times an integer
#' @return object of class "roll" which is a list
roll <- function(die, times = 1) {
  check_times(times)
  object <- list(rolls = sample(die$sides, prob = die$prob, size = times, replace = TRUE),
                 sides = die$sides,
                 prob = die$prob,
                 total = times)
  class(object) <- "roll"
  return(object)
}

#' @title print.roll
#' @description a "print" method for "roll" objects
#' @param roll object of class "roll" which is a list
#' @return displays the class of the object and the generated rolls
print.roll <- function(roll, ...) {
  cat('object "roll"\n\n')
  print(roll$rolls)
  invisible(roll)
}

#' @title summary.roll
#' @description gives the sides, counts, and frequencies of rolls
#' @param roll object of class "roll" which is a list
#' @return object of class "summary.roll" which is a list
summary.roll <- function(roll, ...) {
  rolls <- roll$rolls
  side <- roll$sides
  count <- rep(0, 6)
  for (i in 1:6) {
    count[i] <- sum(side[i] == rolls)
  }
  prop <- count/roll$total
  frequencies <- data.frame(side, count, prop)
  object <- list(freqs = frequencies)
  class(object) <- "summary.roll"
  return(object)
}

#' @title print.summary.roll
#' @description a "print" method for "summary.roll" objects
#' @param x object of class "summary.roll" which is a list
#' @return displays the class of the object and the summary information in a dataframe
print.summary.roll <- function(x, ...) {
  cat('summary "roll"\n\n')
  print(x$freqs)
  invisible(x)
}

#' @title plot.roll
#' @description a plot method for "roll" objects
#' @param roll object of class "roll" which is a list
#' @return a barchart of frequencies
plot.roll <- function(roll, ...) {
  sum_r_e <- summary(roll)
  barplot(height = sum_r_e$freqs[["prop"]],
          names.arg = sum_r_e$freqs[["side"]],
          xlab = "side of die",
          ylab = "relative frequencies",
          main = (sprintf("Relative Frequencies in a series of %s coin tosses", roll$total))
  )
}

make_roll <- function(die, rolls) {
  res <- list(
    rolls  = rolls,
    sides = die$sides,
    prob = die$prob,
    total = length(rolls))
  class(res) <- "roll"
  return(res)
}

"[.roll" <- function(x, i) {
  x$rolls[i]
}

"[<-.roll" <- function(x, i, value) {
  x$rolls[i] <- value
  make_roll(x, x$rolls)
}

"+.roll" <- function(x, incr) {
  if (length(incr) != 1 | incr <= 0) {
    stop("\ninvalid increament (must be positive)")
  }
  more_rolls <- roll(x, times = incr)
  make_roll(x, c(x$rolls, more_rolls$rolls))
}