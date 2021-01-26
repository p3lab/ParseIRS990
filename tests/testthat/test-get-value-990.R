library(testthat)
library(ParseIRS990)
library(purrr)

context("Standaridze an organization's website address (URL)")

test_that("MoveOn example check", {
  
  expect_equal(grepl("http", standardize_url("www.moveon.org")), TRUE)
  
})

context("Find values conditional on parsed return types")

test_that("Check whether it's possible to parse both 990 and 990EZ forms", {
    
    value_list <- map2(rep(c("200067392", "473779347"), each = 3), rep(c("website", "mission_desc", "program_desc"), times = 2), ~get_value_990(get_990(.x), type = .y))

    # Check whether both document forms are parsed 
    expect_equal(length(value_list), 6)
    
    # Check whether any of the results is null (in these cases, all of the outcomes should be false)
    expect_equal(sum(map_int(value_list, is.null)), 0)

})