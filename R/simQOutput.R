#Contains the functions associated with class SimQOutput for the 
#from data part of the project. See class definition below

##' A class containing the expected (and CI) dates for a specific item of interest
##'
##' For example subject recruitment, events occurring, or subject dropout. 
##' Each item of interest will have its own SimQOutput object. This class will
##' be created for the user and does not need to be manually created
##' 
##' As an example median[1] is the expected date of the first specific item of interest
##' whereas upper[1] is the upper CI of the first specific item of interest.
##' @slot median A vector of the expected (i.e median) dates 
##' of the first, second, ..., item of interest
##' The length of the vector is the total number of this type of item. 
##' @slot upper A vector of the expected dates for the upper CI 
##' of the first, second, ..., item of interest
##' @slot lower A vector of the expected dates for the lower CI
##'  of the first, second, ..., item of interest
##' @seealso \code{\link{simulate,EventModel,missing,missing-method}} \code{\link{FromDataResults-class}}
##' @export
setClass("SimQOutput", 
         slots=list(
           upper="Date",
           median="Date",
           lower="Date"
         )
)

# Constructor for \code{SimQOutput} object
# 
# See class description for further details
# @param upper A vector of the upper CI dates
# @param median A vector for the median dates
# @param lower A vector for the lower CI dates 
# @return A \code{SimQOutput} object
SimQOutput <- function(upper,median,lower){
  
  if(length(upper)!= length(median) || length(median) != length(lower)){
    stop("Invalid arguments when creating SimQOutput object")
  }
  
  
  new("SimQOutput",upper=upper,median=median,lower=lower)
  
}


# Create a SimQOutput object from a matrix of (numeric) Dates
# @param details An unsorted matrix with 1 column per subject
# and one row per simulation of numeric Dates
# @param limit limit and 1 - limit will be used as the quantile
# values to be calculated for the lower and upper slot returned SimQOutput object
# @param Nsim The number of rows in \code{details}
# @param event.type An unsorted matrix with 1 column per subject
# and one row per simulation of event.types (integers)
# @param non.inf.event.type Only dates in the details matrix which have event.type=
# non.inf.event.type will be taken into account for the SimQOutput object all others
# will be ignored
# @return A SimQOutput object with the median and quantiles derived from 
# the details matrix
SimQOutputFromMatrix <- function(details,limit,Nsim,event.type=NULL,non.inf.event.type=NULL){
  
  if(!is.null(event.type)){
    details[event.type!=non.inf.event.type] <- as.Date(Inf,origin="1970-01-01")
  }
  
  times <- apply(details,1,sort)
  if(class(times)=="numeric") times <- matrix(times,ncol=Nsim)
  times <- apply(times, 1, stats::quantile, prob=c(limit, 0.5, 1-limit))
  
  ans <- lapply(1:3,function(x){as.Date(times[x,],origin="1970-01-01")})
  
  w <- which(!(ans[[3]]==ans[[2]]& ans[[2]]==ans[[1]] & ans[[1]]==WithdrawnEventDate()))
  
  SimQOutput(
    upper = ans[[3]][w],
    median= ans[[2]][w], 
    lower = ans[[1]][w] 
  )
}



##' @name show
##' @rdname show-methods
##' @aliases show,SimQOutput-method
##' @export
setMethod("show",
          "SimQOutput",
function(object) {
  cat("Lower CI:\n")
  cat(str(object@lower))
  cat("\nMedian:\n")
  cat(str(object@median))
  cat("\nUpper CI:\n")
  cat(str(object@upper))
  cat("\n")
})


# Output the expected time a given number of events occur
# @param event.pred A vector of target event levels
# @param simQ A SimQOutput object (i.e. eventQuantiles slot of WeibullResults)
# @return A data frame containing the median times and confidence intervals for the target event levels
PredictGivenTargetEvents <- function(event.pred,simQ){
  
  if(any(event.pred <= 0 || event.pred > length(simQ@median))) 
    stop("Invalid event.pred value")
  
  ans <- lapply(event.pred,function(x){
    list(time=simQ@median[x],
         event=x,
         CI_low=simQ@lower[x],
         CI_high=simQ@upper[x])    
  })
  
  ans <- do.call(rbind.data.frame, ans)
  ans$time <- as.Date(ans$time,origin="1970-01-01")
  ans$CI_high <- as.Date(ans$CI_high,origin="1970-01-01")
  ans$CI_low <- as.Date(ans$CI_low,origin="1970-01-01")
  ans
}

# Output the expected (median) number of events for a given set of dates
# @param time.pred A vector of dates
# @param simQ A SimQOutput object (i.e. eventQuantiles slot of WeibullResults)
# @return A data frame containing the median  and confidence intervals for the number of events
# at the requested dates
PredictGivenDates <- function(time.pred,simQ){
  
  findEvents <- function(date,event.times){
    if(date < event.times[1]) return(0)
    if(date > event.times[length(event.times)]) return (length(event.times))
    
    start <- 0
    end <- length(event.times)
    
    while(end-start>1){
      centre <- floor((start+end)/2)
      if(date < event.times[centre]){
        end <- centre
      }
      else{
        start <- centre
      }
    }
    return(start)
    
  }
  
  ans <- lapply(time.pred,function(x){
    list(time=x,
         event=findEvents(x,simQ@median),
         CI_low=findEvents(x,simQ@upper), #These are the right way round!
         CI_high=findEvents(x,simQ@lower)
    )    
  })
  ans <- do.call(rbind.data.frame, ans)
  ans$time <- as.Date(ans$time,origin="1970-01-01")
  
  ans
  
}
