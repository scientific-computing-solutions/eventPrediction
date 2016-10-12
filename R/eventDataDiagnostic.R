#The Diagnostic functions (output site/censor information and the lagplot)
#for the predict from data EventData object

##' @include eventData.R
NULL

##' Output a data frame containing all subjects who been censored before
##' a given date
##' 
##' Output a data frame containing all subjects who been censored before
##' a given date not including subjects censored at end of follow up period
##' 
##' @param object An \code{EventData} object
##' @param ... Additional parameters to be passed to the method
##' @return A data frame with the required subject's data 
##' @rdname censorInformation-methods
##' @name censorInformation
##' @export
setGeneric("censorInformation",function(object,...) standardGeneric("censorInformation"))


# Simple internal function for validating the analysis/censor date
# arguments for the Diagnostic functions
# @param date.arg The user's input date
# @param default.date The default date to use if date.arg is null
# @return date.arg or the default.date if date.arg is null
# errors validating date.arg will throw an exception
DiagDataArg <- function(date.arg,default.date){
  if(is.null(date.arg)){
    return(as.Date(default.date,origin="1970-01-01")) 
  }
  FixDates(date.arg)
}


##' @param censor.date All subjects who have a censor date before this
##' date should be output, by default (i.e. when NULL) the latest date for which 
##' any subject information (withdrawal/event/censor) is known
##' @name censorInformation
##' @aliases censorInformation,EventData-method
##' @rdname censorInformation-methods
##'@export
setMethod("censorInformation", "EventData",
  function(object, censor.date=NULL){
    
    censor.date <- DiagDataArg(censor.date,max(LastDate(object@subject.data)))
      
    data <-GetLaggedSubjects(object@subject.data,censor.date)
          
    if(nrow(data)==0){
      return(data.frame(subject=character(0),
                        timelag=numeric(0),
                        rand.date=numeric(0),
                        time=numeric(0)))
    }
            
            
    last.date <- LastDate(data)
    data <- data.frame(subject=data$subject,
                       timelag=as.numeric(censor.date-last.date),
                       rand.date=data$rand.date,
                       time=data$time)
            
    ans <- data[order(data$timelag,decreasing=TRUE),]
    rownames(ans) <- 1:nrow(ans)
    ans
})


##' Output a plot showing the lag between censoring and the date the analysis
##' is being performed
##' @param object An \code{EventData} object
##' @param ... Additional arguments for the function 
##' @rdname DiagnosticPlot-methods
##' @name DiagnosticPlot
##' @export
setGeneric("DiagnosticPlot",function(object,...) standardGeneric("DiagnosticPlot"))



##' @param window.size An optional integer. If used an additional 2 lines at y=x-window.size 
##' and y = x-2*window.size are drawn on the graph. If \code{window.size} is chosen to be the
##' visit schedule (in days) then these lines provide an easy way to determine the number of subjects
##' who have missed one or two visits.
##' @param analysis.date The date the analysis is being perfomed on, by default (i.e. when NULL) it is the
##' the latest date at which any subject is censored/is known to have had an event
##' @param separate.events Logical, if FALSE then all events are coloured the same with label "Had Event", if
##' TRUE then the different event types (object@@subject.data$event.type) are coloured individually.
##' @rdname DiagnosticPlot-methods
##' @aliases DiagnosticPlot,EventData-method
##' @name DiagnosticPlot
##' @export
setMethod("DiagnosticPlot","EventData",
          function(object, window.size=NULL, analysis.date=NULL,separate.events=TRUE){
            if(nrow(object@subject.data)==0)stop("Empty data frame!")    
            
            analysis.date <- DiagDataArg(analysis.date,max(LastDate(object@subject.data)))
            
            xlab <- paste("Days on study if subjects censored on",as.character(analysis.date)) 
            ylab <- "Known days on study"
            
            time.on.study <- object@subject.data$time
            
            
            status <- rep("Ongoing",nrow(object@subject.data))
            status <- ifelse(object@subject.data$has.event==1,
                              if(separate.events)as.character(object@subject.data$event.type) else "Had Event",status)
            status <- ifelse(object@subject.data$censored.at.follow.up==1,"Censored after follow up period",status)
            status <- ifelse(object@subject.data$withdrawn==1,"Withdrawn from Study",status)
            
       
            my.data <- data.frame(subject=object@subject.data$subject,
                                  rand.date=object@subject.data$rand.date,
                                  time.on.study=time.on.study,
                                  t.max=as.numeric(analysis.date - object@subject.data$rand.date + 1,origin="1970-01-01"),
                                  status=status,
                                  site=object@subject.data$site,
                                  date.of.event.censor.or.withdrawal=LastDate(object@subject.data))
            
            
            p <- ggplot(my.data, aes_string(x="t.max", y="time.on.study", color="status")) +
              geom_point() + geom_abline(intercept=0, slope=1, col="black") +
              xlab(xlab) + ylab(ylab)
            
            if(!is.null(window.size)){      
              if(window.size<=0) stop("window.size should be positive")
              p <- p + geom_abline(intercept=-window.size, slope=1, col="black",linetype = 2)+
                geom_abline(intercept=-2*window.size, slope=1, col="black",linetype = 2)
            }
            p     
          }
)

##' Output information about how up to date subject censor dates
##' are for each site
##'
##' Output information about how up to date subject censor dates
##' are for each site in the study subjects who are censored at the 
##' end of their follow up period are
##' not included in this analysis
##' @param object An \code{EventData} object
##' @param ... Additional arguments to be passed to the method
##' @rdname siteInformation-methods
##' @name siteInformation
##' @export
setGeneric("siteInformation",function(object,...) standardGeneric("siteInformation"))

 


##'@param analysis.date The date the analysis is being perfomed on, by default (i.e. when NULL) it is the
##' the latest date at which any subject is censored/is known to have had an event
##'@param ndays The acceptable lag between \code{analysis.date} and censor date. If the lag is 
##'greater than this then the subject will be included in the output data frame.
##'@return A data frame with each row containing a site name and the number
##'of subjects at this site with censor date before \code{analysis.date}-\code{ndays}
##'@rdname siteInformation-methods
##'@aliases siteInformation,EventData-method
##'@name siteInformation
##'@export
setMethod("siteInformation", "EventData",
          function(object, analysis.date=NULL, ndays){
            if(ndays < 0 || class(ndays)!="numeric") stop("ndays must be numeric and non-negative")
            if(all(is.na(object@subject.data$site))) stop("No site information")
            
            analysis.date <- DiagDataArg(analysis.date,max(LastDate(object@subject.data)))
            
            data <-GetLaggedSubjects(object@subject.data,analysis.date-ndays)$site
            
            if(length(data)==0){
              return(data.frame(site=character(0),count=character(0)))
            }
            
            sites <- as.data.frame(table(data))
            colnames(sites) <- c("site","count")
            sites <- sites[order(sites$count,decreasing=TRUE),]
            rownames(sites) <- NULL
            sites[sites$count > 0,]
          })
