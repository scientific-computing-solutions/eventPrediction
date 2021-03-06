% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eventData.R
\name{EventData}
\alias{EventData}
\title{Constructor for event data object}
\usage{
EventData(data, subject, rand.date, has.event, withdrawn, time, site = NULL,
  event.type = NULL, remove.0.time = FALSE, followup = Inf)
}
\arguments{
\item{data}{A data frame}

\item{subject}{string, the column name of subject identifiers}

\item{rand.date}{string, the column name of randomization dates}

\item{has.event}{string, the column name of whether a subject had an event
(1) or not (0)}

\item{withdrawn}{string, the column name of whether a subject has withdrawn
(1) or not (0)}

\item{time}{Either a string, the column name of time each subject has been on the study
or a list with elements with (some of) the following named elements.
\code{last.date}, \code{event.date}, \code{prog.date}, \code{dth.date} and \code{withdrawn.date}
In this case the package will attempt to derive the time column 
See the vignette and then eventPrediction:::AddTimeColumn for further details}

\item{site}{optional column for subject site}

\item{event.type}{optional column for the event type (e.g. unstable angina) if not included then `Had Event' will be used}

\item{remove.0.time}{logical, if TRUE then all subjects with time = NA or 0 are removed from the
data set and not included in the object. If FALSE then they are included in the simulation (but not in the model fitting)}

\item{followup}{A numeric value for the fixed follow up period a subject
is followed. This is in days. If there is no fixed followup period then 
Inf should be used}
}
\value{
An \code{EventData} object
}
\description{
All dates must be in one of the following formats:
YYYY-MM-DD, DD/MM/YY or DD Month YYYY
}

