% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/financial_year.R
\name{financial_year}
\alias{financial_year}
\alias{is_financial_year}
\alias{as_financial_year}
\title{Utilities for working with financial years}
\usage{
financial_year(
  x = integer(),
  boundary = getOption("acadyr.financial_year_boundary", as.Date("2020-04-01"))
)

is_financial_year(x)

as_financial_year(
  x,
  boundary = getOption("acadyr.financial_year_boundary", as.Date("2020-04-01")),
  ...
)
}
\arguments{
\item{x}{\itemize{
\item For \code{financial_year()}: A numeric vector
\item For \code{as_financial_year()}: An object to coerce
\item For \code{is_financial_year()}: An object to test
}}

\item{boundary}{A boundary date where the new financial year should begin.
Note that only the month/day of this date is used. This can be set using
the global option \code{acadyr.financial_year_boundary}. E.g. to use financial
years starting on January 1 (as used by Vietnam, the United Arab Emirates,
Ukraine etc), use the following:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{options(acadyr.financial_year_boundary = as.Date("2020-01-01"))
}\if{html}{\out{</div>}}}

\item{...}{Passed on to methods}
}
\value{
\itemize{
\item For \code{financial_year()}: A \verb{<financial_year>} vector
\item For \code{as_financial_year()}: A \verb{<financial_year>} vector
\item For \code{is_financial_year()}: A \verb{<logical>} vector
}
}
\description{
Utilities for working with financial years
}
\examples{
# Numbers are treated as if they represent years
financial_year(2005:2015)

# Dates are mapped to the year they fall within
dates <- as.Date(c("2020-03-01", "2020-05-01"))
financial_year(dates)

# The year they fall within depends on the year boundary, which
# defaults to April 01
financial_year(dates, boundary = as.Date("2020-01-01"))

# as_financial_year() also understands character vectors:
as_financial_year(c("2020-21", "2021-22"))
}
