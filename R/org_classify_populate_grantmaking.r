
extract_financial_info <- function(xml_plucked, variable){

  xml_plucked %>%
    getNodeSet(variable) %>%
    xmlValue() %>%
    parse_number()

}

get_financial_details_990 <- function(xml_root){

  xml_plucked <- xml_root %>%
    purrr::pluck(2) # pick the second element on the list

  # Current year revenue
  revenue <- xml_plucked %>% extract_financial_info("//CYTotalRevenueAmt")

  # Assets
  assets <- xml_plucked %>% extract_financial_info("//TotalAssetsEOYAmt")

  # Liabilities
  liabilities <- xml_plucked %>% extract_financial_info("//TotalLiabilitiesEOYAmt")

  # Current year expenses
  expenses <- xml_plucked %>% extract_financial_info("//CYTotalExpensesAmt")

  financing <- tibble("Revenue" = revenue,
                      "Assets" = assets,
                      "Liabilities" = liabilities,
                      "Expenses" = expenses)

  return(financing)
}


get_financial_details_990ez <- function(xml_root){

  xml_plucked <- xml_root %>%
    purrr::pluck(2) # pick the second element on the list

  # Current year revenue
  revenue <- xml_plucked %>% extract_financial_info("//TotalRevenueAmt")

  # Assets
  assets <- xml_plucked %>% extract_financial_info("//Form990TotalAssetsGrp/EOYAmt")

  # Liabilities
  liabilities <- xml_plucked %>% extract_financial_info("//SumOfTotalLiabilitiesGrp/EOYAmt")

  # Current year expenses
  expenses <- xml_plucked %>% extract_financial_info("//TotalExpensesAmt")

  financing <- tibble("Revenue" = revenue,
                      "Assets" = assets,
                      "Liabilities" = liabilities,
                      "Expenses" = expenses)

  return(financing)
}


check_for_grantmaking_activity_990 <- function(xml_root) {

  xml_plucked <- xml_root %>%
    purrr::pluck(2) # pick the second element on the list

  if ("IRS990ScheduleI" %in% names(xml_plucked) == TRUE) {
    grantmaking_flag1 <- xml_plucked %>%
  	  getNodeSet("//IRS990ScheduleI//CashGrantAmt") %>%
      xmlSize()

  	grantmaking_flag2 <- xml_plucked %>%
  	  getNodeSet("//TotalGrantOrContriPdDurYrAmt") %>%
  	  xmlSize()

  	return(max(grantmaking_flag1, grantmaking_flag2))

  } else {
    return(c("This organization did not file ScheduleI."))
    }

}



## ------------------------------------


filter_null_grant_info <- function(variable){

  if (length(variable) != 0) {

    variable <- variable %>%
      map(xmlValue) %>%
      map(as.numeric) %>%
      reduce(`+`)} else {

        variable <- 0 # I intentionally made the function to return 0 in this case as it's not NA

      }

}

get_grantmaking_details_990 <- function(xml_root) {

  xml_plucked <- xml_root %>%
    purrr::pluck(2) # pick the second element on the list

  if ("IRS990ScheduleI" %in% names(xml_plucked) == TRUE) {

    grantmaking_total <- xml_plucked %>%
      getNodeSet("//IRS990ScheduleI//CashGrantAmt") %>%
      filter_null_grant_info()

    grantmaking_individuals_total <- xml_plucked %>%
      getNodeSet("//IRS990ScheduleI//GrantsOtherAsstToIndivInUSGrp//CashGrantAmt")  %>%
      filter_null_grant_info()

    grantmaking_individuals_cnt <- xml_plucked %>%
      getNodeSet("//IRS990ScheduleI//GrantsOtherAsstToIndivInUSGrp//RecipientCnt") %>%
      filter_null_grant_info()

    grantmaking_501c3_cnt <- xml_plucked %>%
      getNodeSet("//IRS990ScheduleI//Total501c3OrgCnt") %>%
      map(xmlValue) %>%
      standardize_990_flag()

    grantmaking_other_org_cnt <- xml_plucked %>%
      getNodeSet("//IRS990ScheduleI//TotalOtherOrgCnt") %>%
      map(xmlValue) %>%
      standardize_990_flag()

    other_grantmaking <- xml_plucked %>%
      getNodeSet("//TotalGrantOrContriPdDurYrAmt") %>%
      map(xmlValue)

    if (length(other_grantmaking) == 0) { other_grantmaking <- 0 } # For the same reason above, I think that this should be 0 rather than NA. NA (explicit missing value) should indicate missing values.

    grantmaking_details <- tibble(
      grantmaking_total = grantmaking_total,
      grantmaking_individuals_total = grantmaking_individuals_total,
      grantmaking_orgs_total = grantmaking_total - grantmaking_individuals_total,
      grantmaking_501c3_cnt = grantmaking_501c3_cnt,
      grantmaking_other_org_cnt = grantmaking_other_org_cnt,
      grantmaking_individuals_cnt = grantmaking_individuals_cnt,
      other_grantmaking = other_grantmaking
    )

    return(grantmaking_details)
  }

  else {
    grantmaking_details <- tibble(
      grantmaking_total = NA,
      grantmaking_individuals_total = NA,
      grantmaking_orgs_total = NA,
      grantmaking_501c3_cnt = NA,
      grantmaking_other_org_cnt = NA,
      grantmaking_individuals_cnt = NA,
      other_grantmaking = NA)

    return(grantmaking_details)
  }

}
