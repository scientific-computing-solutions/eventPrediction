##' Expected Progress of a Survival Study 
##' 
##' Calculate the required number of events for a two-arm survival
##' study, and the expected accumulation of events assuming exponential
##' or Weibull survival. Non-uniform accrual may be specified. It is also
##' possible to use real data for predicting the total number of events at a
##' future time point.
##'
##' @name eventPrediction-package
##' @aliases eventPrediction
##' @docType package
##' @title Expected Progress of a Survival Study Assuming Exponential/Weibull Survival
##' @author \email{daniel.dalevi@@astrazeneca.com}
NULL

##' Simulated data that was originally obtained by the exportData function available
##' in the package. The randomization dates have not been modelled properly and is only
##' evenly distributed in the study period. 
##' 
##' @name event.data
##' @docType data
##' @format A data.frame containing a row per patient with six columns:
##' subject patient identifier
##' has.event Logical indicating whether patient has had an event or not
##' event.date Date of event
##' last.date  The date of the last visit
##' rand.date Date when patient was randomized. 
##' eventType For subjects with an event, the type of event which occurred
NULL

##' The S4 show methods for EventPrediction package
##' @name show
##' @rdname show-methods
##' @param object The object to be shown
NULL

##' The S4 summary methods for EventPrediction package
##' @name summary
##' @rdname summary-methods
##' @param object The object for which a summary is desired
NULL


##' The predict methods for EventPrediction package
##' 
##' @name predict
##' @docType methods
##' @rdname predict-methods
##' @param object The object for which the prediction is required
##' @param ... Additional objects
##' @param time.pred If \code{object} is a \code{FromDataResults} object then: 
##' The Dates for which the number of events
##' are required at. This should be a vector of character strings,
##' e.g. c("2017-10-10","2018-04-27) or NULL. 
##' If it is a \code{Study} object then this is a numeric vector of 
##' dates (in months from the start of the trial) for which the number
##' of events are required at.   
##' @param event.pred The target number of events for which the expect time
##' is to be predicted, should be a numeric vector 
##' @return The predicted values. If \code{object} is \code{Study} the result will
##' be an \code{AnalysisResults} object.
##' If \code{object} is a \code{FromDataResults} object it will be
##' returned with \code{event.pred.data} and/or \code{time.pred.data} slots updated 
##' @seealso \code{\link{FromDataResults-class}} \code{\link{AnalysisResults-class}}
##' @export
if(!isGeneric("predict")){
  setGeneric("predict", function(object, ...) standardGeneric("predict"))
}


##' @importFrom graphics plot
NULL 



##' The plot methods for EventPrediction package
##' 
##' For \code{x} an \code{EventData} object, this method will plot
##' a cloglog survival curve of the data. If \code{x} is an \code{EventModel}
##' object, this method will plot a KM curve of the data overlaid with the
##' survival function from the model fit.
##' 
##' @name plot
##' @docType methods
##' @rdname plot-methods
##' @param x Standard arguments to plot generic
##' @param y Standard arguments to plot generic
##' @param ... Additional arguments for the plot function
##' @param show.title Logical, if TRUE, will show title in plot.
##' @param xlab X axis label.
##' @param ylab Y axis label.
##' @param main Plot title
##' @param xlim X axis limits. If \code{x} is \code{FromDataResults} object then
##' c(2,10) will display from 2 to 10 months after the first 
##' subject was recruited. Using c(0,-1) will show the entire graph 
##' @param ylim The y-axis range for the plot
##' @export
setGeneric("plot", function(x, y, ...) standardGeneric("plot"))
