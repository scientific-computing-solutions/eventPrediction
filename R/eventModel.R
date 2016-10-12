# This file contains the public functions associated with
# the fitted Weibull/loglogistic model for the predict from data simulations  

##' @include eventData.R accrual.R fromDataSimInternal.R simQOutput.R ctrlSpec.R singleSimDetails.R fromDataSimParam.R
NULL


##' An S4 class containing a fitted survival model
##' of an Eventdata object
##' @slot model An S3 survreg object
##' @slot event.data An EventData object used to fit a Weibull survial model
##' @slot simParams The \code{FromDataSimParam} object which contains the information
##' needed to generate the survial times
##' @export
setClass("EventModel", 
         slots=list(model="survreg",
                    event.data="EventData",
                    simParams="FromDataSimParam")
         )


##' @name show
##' @rdname show-methods
##' @aliases show,EventModel-method
##' @export
setMethod("show",
  "EventModel",
  function(object) {
    print(object@model)
    
})


##' @param units Scale for the x-axis. "Days", "Months" or "Years"
##' @name plot
##' @rdname plot-methods
##' @aliases plot,EventModel,missing-method
##' @export
setMethod( "plot",
  signature( x="EventModel", y="missing" ),
  function(x, units="Days", xlab=paste("Time in study [",units,"]",sep=""),
           ylab="", main="", ylim=NULL, xlim=NULL, ...) { 
    
    xscale <- GetScaleForKM(units,daysinyear)
    daysinyear <- standarddaysinyear()
    
    KM <- survfit(Surv(time, has.event) ~ 1, data=x@event.data@subject.data,...)
    
    plot(KM, lwd=c(2,1,1), col=c("red", "black", "black" ), 
        xlab=xlab, ylab=ylab,
        xlim=xlim, ylim=ylim,
        main=main,xscale=xscale)
    lines(predict(x@model, type="quantile", p=seq(.01,.99,by=.01))[1,]/xscale, 
         seq(.99,.01,by=-.01), col="brown", type="l", lwd=3)
    
    pos <- if(is.null(xlim)) 0.75*max(KM$time/xscale) else xlim[1] + 0.75*(xlim[2]-xlim[1])
    
    legend(pos, 1, c( "Data", "Model" ), col=c( "red", "brown" ), lty=c(1,1))
  
    }
)


##' Calculate KM risk.table
##' @rdname risk.table-methods
##' @name risk.table
##' @param x An \code{EventModel} object 
##' @param ... Additional arguments to pass to survfit
##' @return A data frame contianing the number at risk at the given times
##' @export
setGeneric("risk.table",
           def=function(x,...)
             standardGeneric("risk.table"))


##' @rdname risk.table-methods
##' @name risk.table
##' @aliases risk.table,EventModel-method
##' @param units Scale for the risk table: "Days", "Months" or "Years"
##' @param risk.times The times for the risk table (in \code{units}). 
##' If NULL then the 0, 0.25,0.5,0.75,1 quantiles of the KM survival table are used
##' @export 
setMethod("risk.table",
  signature("EventModel"),
  function(x,units="Days",risk.times=NULL,...){
    daysinyear <- standarddaysinyear()
    xscale <- GetScaleForKM(units,daysinyear)
    KM <- survfit(Surv(time, has.event) ~ 1, data=x@event.data@subject.data,...)
    if(is.null(risk.times)){
      risk.times <- quantile(c(0, max(KM$time)))/xscale
      names(risk.times) <- NULL
    }  
    
    scaled.risk.times <-risk.times*xscale 
    
    
    KM <- summary(KM,time=scaled.risk.times)
    retVal <- data.frame(time = KM$time/xscale, n.risk = KM$n.risk)
    
      
    n.risk <- unlist(lapply(risk.times,function(x){
                 
                  if(x %in% retVal$time){
                    return(retVal$n.risk[retVal$time==x])
                  }
                  return("NA")
    }))
    
    data <- rbind(risk.times,n.risk)
    rownames(data) <- c("time","n.risk")
    return(data)
   
  }
)


