#This file contains the simulate functions for the predict from
#data part of the package

##' @include eventModel.R
NULL

##' The simulate methods for EventPrediction package 
##' 
##' 
##' These methods are for the predict from data part of the package 
##' and simulate. All functions described here are wrappers for the
##' missing,missing,EventData,FromDataSimParams-method 
##'
##' See the stats::simulate method for details of the stats simulate function 
##'      
##' @name simulate   
##' @param object An \code{EventModel} object which contains both the data (slot event.data) and
##' the simulation parameters (slot simParams). If not stats::simulate will be called 
##' @param data If used then this \code{EventData} object will be used when performing the simulations
##' instead of the \code{EventData} object within \code{fit}
##' @param SimParams A \code{FromDataSimParam} object, 
##' overrides the simulated parameters from the \code{EventModel} object
##' @rdname simulate-methods
##' @param accrualGenerator An AccrualGenerator object used for recruiting additional subjects 
##' @param Naccrual The number of additional subjects to be recruited
##' @param Nsim Number of simulations to run 
##' @param limit Limit for the percentiles, default is 0.05 which corresponds
##' to [0.05, 0.95]
##' @param seed Integer for random number generator (for reproducability) By default NULL.
##' @param longlagsettings A LongLagSettings object to control the behaviour of the algorithm for subjects whos last date
##' is a long time from the analysis date. Using this argument can have a large impact on the results - see vignette for further details
##' @param HR The hazard ratio: an advanced option which allows two arm trials to be simulated. This replicates the 
##' Predict from parameters functionality but uses the recruitment times found in \code{data}. See the vignette for
##' further details    
##' @param r The allocation ratio: see \code{HR} argument.
##' @param dropout if subjects drop out in study (due to competing risks not as there is a finite follow up time)
##' then this argument should contain a list with proportion and time and optionally shape i.e.
##' \code{dropout=list(proportion=0.03,time=365,shape=1.2)} meaning in the absence of events 3% of subjects
##' will have dropped out after 365 days with a Weibull hazard rate with shape=1.2. If shape is not included then 
##' it defaults to 1 (exponential rate). If dropout is NULL then no subjects will drop out
##' @param ... Additional arguments to be passed to the method
##' @docType methods
##' @return A \code{FromDataResults} object
##' @export
if(!isGeneric("simulate")){
  setGeneric("simulate", function(object,data,SimParams,...) standardGeneric("simulate"))
}



##' @rdname simulate-methods
##' @name simulate
##' @aliases simulate,ANY,missing,missing-method
##' @export 
setMethod("simulate",signature=c("ANY","missing","missing"),
  function(object,...){
    stats::simulate(object=object,...)            
})


##' @rdname simulate-methods
##' @name simulate
##' @aliases simulate,EventModel,missing,missing-method
##' @export
setMethod("simulate",signature=c("EventModel","missing","missing"),function(object,...){
  simulate(data=object@event.data,SimParams=object@simParams,...)
})


##' @rdname simulate-methods
##' @name simulate
##' @aliases simulate,EventModel,EventData,missing-method
##' @export
setMethod("simulate",signature=c("EventModel","EventData","missing"),function(object,data,...){
  if(data@followup!=object@event.data@followup){
    warning("Model data followup does not equal EventData followup. EventData followup will be used")
  }
  simulate(data=data,SimParams=object@simParams,...)
})

##' @rdname simulate-methods
##' @name simulate
##' @aliases simulate,EventModel,missing,FromDataSimParam-method
##' @export
setMethod("simulate",signature=c("EventModel","missing","FromDataSimParam"),function(object,SimParams,...){
  simulate(data=object@event.data,SimParams=SimParams,...)
})


##' @rdname simulate-methods
##' @name simulate
##' @aliases simulate,missing,EventData,FromDataSimParam-method
##' @export
setMethod("simulate",signature=c("missing","EventData","FromDataSimParam"),
  function(data,SimParams,accrualGenerator=NULL,Naccrual=0, Nsim=1e4, seed=NULL, limit=0.05, 
           longlagsettings=NULL,HR=NULL,r=NULL,dropout=NULL){

  #validate the arguments
  validate.simulate.arguments(accrualGenerator,Naccrual,Nsim,seed,
                              limit,longlagsettings,HR,r,data)  

  #calculate the dropout rate and shape for drop out
  dropoutctrlSpec <- CtrlSpecFromList(dropout,eventtext="",1)[[1]]
  dropout.shape <- if(is.null(dropout) || is.null(dropout$shape)) 1 else dropout$shape
  dropout.rate <- log(2)^(1/dropout.shape)/dropoutctrlSpec@median
    
  #set seed to be used
  if(!is.null(seed)) set.seed(seed)
 
  #pre-process data to deal with subjects censored
  #a long time in the past
  indat <- DealWithReportingLag(data@subject.data,longlagsettings)      
 
  #create matrix of subject recruitment times including additional
  #accrual we have a matrix with 1 row per simulation, 1 column per subject
  rec.details <- CalculateAccrualTimes(Naccrual,Nsim,indat$rand.date,accrualGenerator) 
  
  #calculate quantiles from the recruitment details matrix for storing in output 
  recQuantiles <- SimQOutputFromMatrix(rec.details,limit,Nsim)
  
  #subset the recruitment details to get the new subjects        
  newrecs <- if(Naccrual!= 0) rec.details[,(ncol(rec.details)-Naccrual+1):ncol(rec.details)]
             else NULL
  
  #generate the simulation specific parameters
  #e.g. rate and shape Weibull parameters used for each simulation 
  singleSimParams <- SimParams@generateParameterFunction(Nsim)
  
  #perform the simulations    
  outcomes <-apply(singleSimParams, 1, PerformOneSimulation,
                   number.subjects=nrow(indat),
                   Naccrual=Naccrual,
                   indat=indat,newrecs=newrecs,HR=HR,r=r,
                   dropout.rate=dropout.rate, 
                   dropout.shape=dropout.shape, followup=data@followup,
                   conditionalFunction=SimParams@conditionalFunction)
  
  #post process the output
  event.type <- sapply(outcomes,function(x){x$event.type})
  if(class(event.type)=="numeric") event.type <- matrix(event.type,ncol=Nsim)
  times <- sapply(outcomes,function(x){x$event.times})
  if(class(times)=="numeric") times <- matrix(times,ncol=Nsim)
  
  #calculate the quantiles for the events and dropouts
  event.times <- t(times)
  eventQuantiles <- SimQOutputFromMatrix(event.times,limit,Nsim,event.type=t(event.type),non.inf.event.type=0)
  dropoutQuantiles <- SimQOutputFromMatrix(event.times,limit,Nsim,event.type=t(event.type),non.inf.event.type=1)
  
  #use a dummy AccrualGenerator if one not given
  if(is.null(accrualGenerator)) 
    accrualGenerator <-  new("AccrualGenerator",f=function(N){NULL},model="NONE",text="NONE")
  
  return(new("FromDataResults", 
             eventQuantiles = eventQuantiles,  
             recQuantiles=recQuantiles,
             limit = limit,
             event.data = data,
             accrualGenerator=accrualGenerator,
             Naccrual=Naccrual,
             time.pred.data=EmptyPredictionDF(),
             event.pred.data=EmptyPredictionDF(),
             singleSimDetails=SingleSimDetails(event.type=event.type,event.times=times,rec.times=t(rec.details)),
             dropout.shape=dropout.shape,
             dropout.rate=dropout.rate,
             dropoutQuantiles=dropoutQuantiles,
             simParams=SimParams
  ))  
  
              
})



