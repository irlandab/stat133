# load the source code of the functions to be tested
source("functions.R")

# context with one test that groups expectations
context("Test for range value") 

test_that("range works as expected", {
  x <- c(1, 2, 3, 4, 5)
  
  expect_equal(stat_range(x), 4)
  expect_length(stat_range(x), 1)
  expect_type(stat_range(x), 'double')
})

test_that("range value for numeric vectors with NAs", {
  y <- c(1, 2, 3, 4, NA)
  
  expect_length(stat_range(y), 1)
  expect_equal(stat_range(y), NA_real_)
})

test_that("range value for logical vectors", {
  z <- c(TRUE, FALSE, TRUE)
  
  expect_length(stat_range(z), 1)
  expect_type(stat_range(z), "integer")
  expect_equal(stat_range(z), 1L)
})

test_that("range value for non-numeric vectors", {
  w <- letters[1:5]
  
  expect_error(stat_range(z))
})


context("Test for center values") 

test_that("centers works as expected", {
  x <- c(1, 2, 3, 4, 5)
  
  expect_equal(stat_centers(x), c("Median" = 3, "Mean" = 3))
  expect_length(stat_centers(x), 2)
  expect_type(stat_centers(x), 'double')
})


context("Test for spread values") 

test_that("centers works as expected", {
  x <- c(1, 2, 3, 4, 5)
  
  expect_length(stat_spreads(x), 3)
  expect_type(stat_spreads(x), 'double')
})