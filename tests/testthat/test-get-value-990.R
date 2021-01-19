library(testthat)
library(ParseIRS990)

context("Standaridze an organization's website address (URL)")

test_that("MoveOn example check", {
    
  idx <- import_idx(2019)
  xml_root <- get_990("061553389")
  expect_condition 
  expect_equal(grepl("http", get_value_990(xml_root, type = "website")) == TRUE, TRUE)    
  
})