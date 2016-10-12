#The DisplayOptions class definition
#This object is used to specify the
#predict form parameters summary text

##' Class defining display options for predict from parameter graphs
##' @slot Time string displaying the time units
##' @slot Title string title to be displayed on the graph
##' @slot StartDate if `0' the x-axis is labelled using 0,1,2,...
##' if a date is used then the x-axis is labelled with dates 
##' @slot Control legend text for control arm
##' @slot Exp legend text for experimetal arm
##' @slot Trecruit logical in \code{summary} output number recruited
##' @slot Tratio logical in \code{summary} output recruitment ratio
##' @slot Tacc logical in \code{summary} output recruit details
##' @slot Tmedian logical in \code{summary} output control median
##' @slot Thr logical in \code{summary} output hazard ratio
##' @slot Tcrithr logical in \code{summary} output critical hazard ratio
##' @slot text.width numeric in \code{summary} the width for the summary text 
##' @slot ShowRec logical. Show the number of subjects recruited at target times
##' @slot Dropout logical. Show the drop out details
##' @slot atRiskTime string displayin the units of at risk,
##' @slot atRiskConversion numeric, factor for converting Time into atRiskTime
##' @slot showatRisk logical should the at risk details be output
##' @export
setClass( "DisplayOptions", 
          slots=c( 
            Time = "character",
            Title = "character",
            StartDate = "character",
            Control = "character",
            Exp = "character",
            Trecruit = "logical",
            Tratio = "logical",
            Tacc = "logical",
            Tmedian = "logical",
            Thr = "logical",
            Tcrithr = "logical",
            text.width="numeric",
            ShowRec="logical",
            Dropout="logical",
            atRiskTime = "character",
            atRiskConversion="numeric",
            showatRisk="logical"),        
          prototype = prototype( 
            Time = "months",
            Title = "Expected Recruitment and Events",
            StartDate = "0",
            Control = "Control",
            Exp = "Experimental",
            Trecruit = TRUE, 
            Tratio = TRUE, 
            Tacc = TRUE, 
            Tmedian = TRUE, 
            Thr = TRUE,
            Tcrithr = TRUE,
            text.width=75,
            ShowRec=FALSE,
            Dropout=TRUE,
            atRiskTime = "years",
            atRiskConversion=1/12,
            showatRisk=FALSE) 
)

##' DisplayOptions constructor
##' @param ... parameters passed to new
##' @return A new DisplayOptions object
##' @seealso \code{\link{DisplayOptions-class}}
##' @export
DisplayOptions <- function(...){
  new("DisplayOptions",...)
}