#Contains the functions for SingleSimDetails class
#part of the predict from data part of the package
#see class definition below

##' Class containing the subject level event data
##' for each simulation
##' 
##' A class which contains the recruitment times
##' event times and event types of all subjects and all
##' simulations for a predict from data simulation. 
##' This class is created when performing simulation and does not 
##' need to be constructed manually 
##'  
##' @slot event.type A matrix (1 row per subject, 1 column per simulation) of
##' the reason subjects leave trial. 0 is have event, 1 is drop out and 2 is drop out due to follow up
##' @slot event.times A matrix (1 row per subject, 1 column per simulation) of
##' dates (as numeric) subjects leave the trial
##' @slot rec.times A matrix (1 row per subject, 1 column per simulation) of
##' recruitment dates (as numeric). 
##' Therefore event.times[i,j] - rec.times[i,j] + 1 is the time on study for
##' subject i in simulation j
##' @export
setClass("SingleSimDetails", 
         slots=list(
           event.type="matrix",
           event.times="matrix",
           rec.times="matrix"
         )
) 


# Constructor of SingleSimDetails object
# @param event.type A matrix (1 row per subject, 1 column per simulation) of
# the reason subjects leave trial. 0 is have event, 1 is drop out and 2 is censored due to 
# completing follow up period
# @param event.times A matrix (1 row per subject, 1 column per simulation) of
# dates (as numeric) subjects leave the trial
# @param rec.times A matrix (1 row per subject, 1 column per simulation) of
# recruitment dates (as numeric). 
# Therefore event.times[i,j] - rec.times[i,j] + 1 is the time on study for
# subject i in simulation j
SingleSimDetails <- function(event.type,event.times,rec.times){
  
  new("SingleSimDetails",event.type=event.type,
      event.times=event.times,rec.times=rec.times)
  
}



##' @name CalculateDaysAtRisk
##' @rdname CalculateDaysAtRisk-methods  
##' @aliases CalculateDaysAtRisk,SingleSimDetails-method
##' @export
setMethod("CalculateDaysAtRisk","SingleSimDetails",
 function(object,times){
  
  unlist(lapply(times,function(x){
    cutoff <- as.numeric(x,origin="1970-01-01")
    time.at.risk <-  matrix(pmax(0, pmin(cutoff,object@event.times) - object@rec.times + 1),
                            ncol=ncol(object@event.times))
    median(apply(time.at.risk,2,sum))
  }))
  
})