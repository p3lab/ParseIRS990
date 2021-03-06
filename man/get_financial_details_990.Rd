% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/org_classify_populate_grantmaking.r
\name{get_financial_details_990}
\alias{get_financial_details_990}
\title{Get financial details from 990 forms}
\usage{
get_financial_details_990(xml_root)
}
\arguments{
\item{xml_root}{An XML root element associated with a particular organization}
}
\value{
If successful, the function returns a dataframe that contains information on an organization's "revenue," "assets," "liabilities," and "expenses."
}
\description{
Get financial details from 990 forms
}
