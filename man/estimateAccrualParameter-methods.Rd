% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accrual.R
\docType{methods}
\name{estimateAccrualParameter}
\alias{estimateAccrualParameter}
\alias{estimateAccrualParameter,EventData-method}
\title{Generic function to estimate non-uniformity accrual 
parameter \code{k}}
\usage{
estimateAccrualParameter(object, ...)

\S4method{estimateAccrualParameter}{EventData}(object,
  start.date = min(object@subject.data$rand.date),
  end.date = max(object@subject.data$rand.date))
}
\arguments{
\item{object}{An \code{EventData} object}

\item{...}{Additional arguments to be passed to specific method}

\item{start.date}{The startdate for recruitment, by default the date 
the first subject is recruited. If subjects are recruited on start.date, it is
assumed they are recruited on the following day to avoid log(0) in the 
maximum likelihood estimate.}

\item{end.date}{The end date for recruitment, by default the date 
the last subject is recruited.}
}
\value{
A maximum likelihood estimate for k, see vignette 
for further details
}
\description{
Note The estimate produced assumes recruitment has completed.
}

