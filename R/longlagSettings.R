# This file contains the class and its constructor which
# determine how the simulation (from data) procedure deals with subjects
# who are censored a long time before the analysis date.

##' Class which determines how the simulation (from data) 
##' procedure deals with subjects
##' who are censored a long time before the analysis date.
##' @slot analysis.date The date the event prediction is performed on. If \code{as.Date(NA)}
##' then the latest censor/event date from the data set will be used as the analysis.date
##' @slot ndays All subjects who were censored more than \code{ndays} before
##' \code{analysis.date} will be changed
##' @slot toWithdraw Logical. If TRUE then the selected subjects will be withdrawn
##' rather than be censored. If FALSE, their censor date is replaced by \code{analysis.date}
##' @slot visitschedule number of days between visits for pfs data. If non-zero and 
##' \code{toWithdraw} is TRUE then the selectedsubjects are censored on the date their
##' latest visit would have been. See vignette for further details
##' @slot text The text to be disaplyed when printing the LongLagSettings 
##' @seealso \code{\link{show,LongLagSettings-method}}
##' @export
setClass("LongLagSettings", 
         slots= list(analysis.date="Date",
                     ndays="numeric",
                     toWithdraw="logical",
                     visitschedule="numeric",
                     text="character")
)



##' @name show
##' @rdname show-methods
##' @aliases show,LongLagSettings-method
##' @export
setMethod("show", signature(object="LongLagSettings"),
  function(object) {
    cat(object@text)
  }
)          


##' Constructor for \code{LongLagSettings} object
##' @param analysis.date The date the event prediction is performed on. If \code{as.Date(NA)}
##' then the latest censor/event date from the data set will be used as the analysis.date
##' @param ndays All subjects who were censored more than \code{ndays} before
##' \code{analysis.date} will be changed
##' @param toWithdraw Logical. If TRUE then the selected subjects will be withdrawn
##' rather than be censored. If FALSE, their censor date is replaced by \code{analysis.date}
##' @param visitschedule number of days between visits for pfs data. If non-zero and 
##' \code{toWithdraw} is TRUE then the selectedsubjects are censored on the date their
##' latest visit would have been. See vignette for further details
##' @return A \code{LongLagSettings} object
##' @export
LongLagSettings <- function(analysis.date=as.Date(NA),ndays,toWithdraw,visitschedule=0){
   
  if(visitschedule < 0 || length(visitschedule) > 1) stop("invalid visit schedule")
  if(ndays < 0 || length(ndays) > 1) stop("invalid ndays")
  
  analysis.date <- FixDates(analysis.date)
  
  
  text <- if(ndays!=0) paste("Subjects with censor date more than",ndays,"days before",analysis.date)
          else text <- paste("Subjects with censor date before",analysis.date)
  
  
  if(toWithdraw){
    text <- paste(text, "are withdrawn.")
  }
  else{
    if(visitschedule==0){
      text <- paste(text," are now censored on ",analysis.date,".",sep="")
    }
    else{
      text <- paste(text," are now censored at the last expected visit date before ",analysis.date,
                    ", with a visit schedule of ",visitschedule," days." ,sep="")
    }
  }
  
  new("LongLagSettings",analysis.date=analysis.date,
      ndays=ndays,toWithdraw=toWithdraw,visitschedule=visitschedule,
      text=text)
  
}


# Function to handle subjects who are
# censored a long time before the analysis date.
# 
# @param indat A data frame e.g. EventData@@subject.data
# @param longlagsettings A \code{longlagsettings} object which controls 
# how subjects who are censored a long time before the analysis date
# are handled. 
# @return The data frame with the \code{longlagsettings} applied 
DealWithReportingLag <- function(indat,longlagsettings){
  
  if(is.null(longlagsettings)){
    return(indat)
  }
  
  
  last.date <- LastDate(indat)
  analysis.date  <- if(is.na(longlagsettings@analysis.date)) as.Date(max(last.date),origin="1970-01-01",na.rm=TRUE) 
                    else longlagsettings@analysis.date 
    
  idx.to.cons <- indat$censored.at.follow.up==0  & indat$withdrawn==0  &  indat$has.event == 0 & 
                (last.date < (analysis.date - longlagsettings@ndays))
    
  if(longlagsettings@toWithdraw){
    indat$withdrawn[idx.to.cons] <- 1 #time of withdrawal is unchanged
    return(indat)
  }
  
  indat$time[idx.to.cons] <- as.numeric(analysis.date - indat$rand.date[idx.to.cons]+1)
  if(longlagsettings@visitschedule != 0){
    indat$time[idx.to.cons] <- 
      1+floor((indat$time[idx.to.cons]-1)/longlagsettings@visitschedule) * longlagsettings@visitschedule    
  }
  indat
  
}