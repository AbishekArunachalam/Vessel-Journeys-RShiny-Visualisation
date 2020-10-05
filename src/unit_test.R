library(testthat)

source('server.R')

test_that('journey is not a data frame', {
  expect_that(is.data.frame(calculate_max_distance('Navigation', 'BY HEL')$journey), equals(TRUE))
})

test_that('max distance value is not present', {
  expect_that(!is.na(calculate_max_distance('Navigation', 'BY HEL')$max_distance_covered), equals(data.frame()))
})

test_that('num of elements is not correct', {
  expected <- 3
  actual <- length(calculate_max_distance('Navigation', 'BY HEL'))
  expect_equal(expected, actual)
})

