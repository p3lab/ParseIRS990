% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/org_classify_description_texts.R
\name{get_value_990}
\alias{get_value_990}
\title{Get concrete information from 990 forms}
\usage{
get_value_990(
  xml_root,
  type = c("website", "mission_desc", "program_desc"),
  text_length_threshold = 50
)
}
\arguments{
\item{xml_root}{An XML root element associated with a particular organization}

\item{type}{A type of concrete information. It should be either "website" (website URL), "mission_desc," (mission statement) or "program_desc" (program description)}

\item{text_length_threshold}{The length of the minimum words associated with a particular organization. This value is used to filter the program descriptions. The default value is 50.}
}
\value{
Depending on the type parameter, the function returns either a website URL, a mission statement, or a program description(s).
}
\description{
Get concrete information from 990 forms
}
