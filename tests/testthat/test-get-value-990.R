library(testthat)
library(ParseIRS990)

context("Standaridze an organization's website address (URL)")

test_that("MoveOn example check", {
  
  expect_equal(grepl("http", standardize_url("www.moveon.org")), TRUE)
  
})