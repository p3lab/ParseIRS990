library(testthat)
library(ParseIRS990)

context("Standaridze an organization's website address (URL)")

test_that("MoveOn example check", {
  
  expect_equal(grepl("http", standardize_url("www.moveon.org")), TRUE)
  
    })

grepl("http", standardize_url("www.moveon.org"))
test_that("Check whether the exact document source is provided when inspecting Schedule O", {
    
    # 2019 IRS data 
    data("idx_2019")
    
    expect_equal(unique(grepl("From FORM", get_scheduleO("061553389"))), TRUE)

    })

test_that("Check whether get_scheduleO returns all entries", {
  
expect_equal(length(grepl("From FORM", (get_scheduleO("061553389")), TRUE)) >= 2, TRUE)
  
  })