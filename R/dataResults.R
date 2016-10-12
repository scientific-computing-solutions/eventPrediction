# This file contains the public functions associated
# with the results of the predict from data simulations

##' @include eventData.R accrual.R longlagSettings.R simQOutput.R singleSimDetails.R eventPrediction_package.R
NULL

##' Class that contains results from the simulating event predictions
##' from Data
##' @slot limit The confidence interval width used by recQuantiles, eventQuantiles and dropoutQuantiles
##' If limit = 0.05 then the CI is the 5th - 95th percentile
##' @slot eventQuantiles The dates on which the median and CI of each event is expected to occur
##' @slot event.data The EventData object used when simulating events
##' @slot accrualGenerator The AccrualGenerator object used to recruit new subjects
##' @slot Naccrual The number of additional subjects recruited for each simulation
##' @slot time.pred.data A data frame giving the expected number of events (and CI intervals) for
##' given target dates. See  vignette for further details
##' @slot event.pred.data A data frame giving the expected date (and CI intervals) for given 
##' target numbers of events occurring. See vignette for further details
##' @slot recQuantiles The dates on which the median and CI of each recruitment is expected to occur
##' @slot dropoutQuantiles The dates on which the median and CI of each event is expected to occur
##' @slot singleSimDetails A SingleSimDetails object containing the subject level details of the simulation
##' see SingleSimDetails class documentaion for further details.
##' @slot dropout.shape The Weibull shape parameter of the dropout risk used for the simulations
##' @slot dropout.rate The Weibull rate parameter of the dropout risk used for the simulations (in units of day^{-1}).
##' If no dropout then this is 0
##' @slot simParams The \code{FromDataSimParam} object which contains the information
##' used to generate the survial times
##' @seealso \code{\link{show,FromDataResults-method}}, \code{\link{predict,FromDataResults-method}},
##' \code{\link{summary,FromDataResults-method}} 
##' @export
setClass("FromDataResults",
         slots=list(limit = "numeric",
                    eventQuantiles="SimQOutput",
                    event.data = "EventData",
                    accrualGenerator="AccrualGenerator",
                    Naccrual="numeric",
                    time.pred.data="data.frame",
                    event.pred.data="data.frame",
                    recQuantiles = "SimQOutput",
                    dropoutQuantiles="SimQOutput",
                    singleSimDetails="SingleSimDetails",
                    dropout.shape="numeric",
                    dropout.rate="numeric",
                    simParams="FromDataSimParam") 
) 


##' @name show
##' @rdname show-methods
##' @aliases show,FromDataResults-method
##' @export
setMethod( "show",
           "FromDataResults",
           function(object) {
             cat("FromDataResults object, use object@param to access individual columns:\n")
             cat("limit: ")
             cat(object@limit)
             cat("\nEvent Times\n")
             show(object@eventQuantiles)
             cat(".. use object@EventData@param to access individual columns:\n")
             cat(str( object@event.data@subject.data)) 
           })



##' @name predict
##' @rdname predict-methods
##' @aliases predict,FromDataResults-method
##' @export
setMethod("predict","FromDataResults",
function(object,time.pred=NULL,event.pred=NULL){
    
  if(is.null(time.pred) && is.null(event.pred)){
    stop("No predictions requested! Please enter either an event.pred or time.pred argument")
  }
  
  max.num.events <- length(object@eventQuantiles@median)
  
  if(any(event.pred <= 0) || any(event.pred > max.num.events)){
    stop(paste("Invalid event.pred must be positive and < ",max.num.events+1,
               ". Fewer than ", 100*object@limit ,"% of the simulations had at least " ,max.num.events+1, " events.",sep=""))
  }
  
  if(!is.null(time.pred)){
    time.pred <- FixDates(time.pred)
    new.details <- PredictGivenDates(time.pred,object@eventQuantiles)
    new.details$daysatrisk <- CalculateDaysAtRisk(object@singleSimDetails,new.details$time)
    object@time.pred.data <- rbind(object@time.pred.data,new.details)
    rownames(object@time.pred.data) <- NULL
  } 
  
  if(!is.null(event.pred)){
    new.details <- PredictGivenTargetEvents(event.pred,object@eventQuantiles)
    new.details$daysatrisk <- CalculateDaysAtRisk(object@singleSimDetails,new.details$time)
    object@event.pred.data <- rbind(object@event.pred.data,new.details)
    rownames(object@event.pred.data) <- NULL
  } 
  
  return(object)
})




##' @param round.method If the string "toMonths" then dates are rounded 
##' to the nearest month. For the lower confidence interval value the month of 
##' the date 15 days earlier than the given value is used
##' and for the upper confidence interval value the month of the
##' date 15 days later is used. For the median, the month is used.    
##' @param text.width The width of the summary text
##' @param show.predictions Logical if TRUE then include the time.pred.data and
##' event.pred.data information in the text 
##' @param show.at.risk Output the median number of at risk years 
##' @rdname summary-methods
##' @name summary
##' @rdname summary-methods
##' @aliases summary,FromDataResults-method
##' @export
setMethod("summary","FromDataResults",
function(object,round.method="None",text.width=60,show.predictions=TRUE,show.at.risk=TRUE){
  cat(getFromDataResultsSummaryText(object,round.method,text.width,show.predictions,show.at.risk))
})



##' @param title The title text, by default it is \code{summary(x)} 
##' @param text.width Number of characters to wrap title text by.
##' @param show.obs If TRUE, add a line and points displaying the observed events.
##' @param round.method If the string "toMonths" then dates are rounded 
##' to the nearest month. For the lower confidence interval value 
##' the date 15 days earlier than the given value is rounded to the nearest 
##' month and for the upper confidence intervalvalue the
##' date 15 dats later than the given value is rounded to the nearest month  
##' @param show.predictions If TRUE show brown dashed lines at the user specified prediction
##' times/number of events.
##' @param pred.to.present If TRUE, move all predictions from past to present. Beware.
##' @param include.dropouts Include the cumulative dropouts on the graph (logical)
##' @param legend.position The position of the graph legend
##' @param custom.dates A vector of dates (in string or Date format) to be output on the x-axis
##' if NULL then default dates will be used
##' @rdname plot-methods
##' @name plot
##' @aliases plot,FromDataResults,missing-method
##' @export
setMethod( "plot",
           signature( x="FromDataResults", y="missing" ),
           function( x, title=getFromDataResultsSummaryText(x,round.method=round.method,text.width=text.width,
                                                           show.predictions=show.predictions), 
                     show.title=FALSE, text.width=80, show.obs=FALSE,round.method="None",show.predictions=TRUE, 
                     pred.to.present = FALSE, xlim = c(0,-1), ylim=NULL,include.dropouts=TRUE,
                     legend.position="bottomright",custom.dates=NULL) {
             
             
             daysinyear <- standarddaysinyear()
             
             #Note this function is due a refactoring
             #as the rec, event and dropout lines could all call
             #the same function and the
             #event and dropout data could call the same function
             
             if(pred.to.present){
               warning("Using pred.to.present may mask problems with the data. This option is not recommended")
             }
             
             
             #First deal with margins
             oldmar <- par()$mar
             
             mar_val <- 0.5
             if(show.title){
               chr.pos <- which(unlist(strsplit(title,NULL)) == '\n') 
               chr.count <- length(chr.pos) 
               mar_val <- 1.5+chr.count
               
             }
             par(mar=c(5.1,4.1,mar_val,2.1))
            
             #real data for past events
             indat <- x@event.data@subject.data
             indat$last.date <- LastDate(indat) 
             eventdata <- indat[indat$has.event==1, ]                       
             eventdata <- eventdata[order(eventdata$last.date),]
             dropoutdata <- indat[indat$withdrawn==1,]
             dropoutdata <- dropoutdata[order(dropoutdata$last.date),]
             
             inf.date <- as.Date(Inf,origin="1970-01-01")
             
             q.median <- x@eventQuantiles@median[x@eventQuantiles@median!=inf.date]
             q.upper <- x@eventQuantiles@upper[x@eventQuantiles@upper!=inf.date] 
             q.lower <- x@eventQuantiles@lower[x@eventQuantiles@lower!=inf.date]
                      
             
             
             #Sort out ranges
             x_min <- min(x@recQuantiles@median) + xlim[1]*daysinyear/12
             x_max <- if(xlim[2]>0) min(x@recQuantiles@median) + xlim[2]*daysinyear/12 else max(q.upper,q.median,q.lower)           
             
             r <- c(x_min,x_max)
                                 
                             
             N.subjects <- length(indat$rand.date) 
                   
                 
             if(pred.to.present && N.subjects > 0 && nrow(eventdata) > 0){
               idx <- which(q.median < max(indat$last.date))
               q.median[nrow(eventdata):max(idx)] <- as.Date(max(indat$last.date),origin="1970-01-01")
               q.lower[idx] <- q.median[idx]
               q.upper[idx] <- q.median[idx]
             }
             
             if(is.null(ylim)) ylim <- c(0, N.subjects + x@Naccrual)
             
             plot(q.median, seq_along(q.median), type='l',
                  xlim=r,
                  ylim=ylim,
                  xlab="",
                  ylab="",
                  col="blue", 
                  lwd = 2,
                  xaxt = "n",
                  las = 1
             )
             
             if(nrow(eventdata)> 0){                          
               lines(eventdata$last.date, 1:nrow(eventdata), col="purple", lwd=2)
               lines(c(tail(eventdata$last.date,1),max(indat$last.date)),rep(nrow(eventdata),2),col="purple",lwd=1,lty=1)
             } 
             if(nrow(dropoutdata)>0 && include.dropouts){
               lines(dropoutdata$last.date, 1:nrow(dropoutdata), col="tan", lwd=2 )
               lines(c(tail(dropoutdata$last.date,1),max(indat$last.date)),rep(nrow(dropoutdata),2),col="tan",lwd=1,lty=1)
             } 
             if(show.obs == TRUE){
               if(nrow(eventdata) > 0) points( eventdata$last.date, 1:nrow(eventdata), col="purple",  pch=20 )
               if(nrow(dropoutdata)>0 && include.dropouts) points( dropoutdata$last.date, 1:nrow(dropoutdata), col="tan",  pch=20 )
             }
             
             
             qs.mod <- if(is.null(custom.dates))    
                          seq(from=r[1], to=r[2], by=(r[2]-r[1])/25 )
                       else
                         FixDates(custom.dates)
             
             axis(1, qs.mod, format(qs.mod, "%d %b %Y"), cex.axis = .7, las=3)
             
             
             mtext('N',side=2,at=median(seq_along(q.lower)),line=3, las=1)
             lines(q.lower, seq_along(q.lower), type='l', lty='dashed', col="red", lwd = 2)
             lines(q.upper, seq_along(q.upper), type='l', lty='dashed', col="red", lwd = 2)
             lines(x@recQuantiles@median, 1:length( x@recQuantiles@median), lwd=2 )
             lines(c(tail(x@recQuantiles@median,n=1),max(tail(x@recQuantiles@median,n=1),r[2])),rep(N.subjects+x@Naccrual,2),lwd=2)
             
             lines(x@recQuantiles@lower, 1:length(x@recQuantiles@lower), lwd=1,lty="dashed")
             lines(x@recQuantiles@upper, 1:length(x@recQuantiles@upper), lwd=1,lty="dashed")
            
             if(include.dropouts && x@dropout.rate!=0){
               lines(x@dropoutQuantiles@median,seq_along(x@dropoutQuantiles@median),type='l',col="aquamarine4",lwd=2)
               lines(x@dropoutQuantiles@lower, 1:length(x@dropoutQuantiles@lower), lwd=1,lty="dashed",col="aquamarine3")
               lines(x@dropoutQuantiles@upper, 1:length(x@dropoutQuantiles@upper), lwd=1,lty="dashed",col="aquamarine3")
             }
             
             
             if(nrow(eventdata)>0){
               points(max(indat$last.date, na.rm=TRUE), nrow(eventdata), cex=1.5, pch=21, col="red", bg="yellow")
               abline(h=nrow(eventdata), lty="dashed", col="blue")
             }
             abline(v=max(indat$last.date, na.rm=TRUE), lty="dashed", col="blue") 
             if(nrow(dropoutdata)>0){
               points(max(indat$last.date, na.rm=TRUE), nrow(dropoutdata), cex=1.0, pch=21, col="tan", bg="darkslategray") 
             }
             
             
             if(show.title==TRUE){
               mtext(title,side=3,cex=0.9,adj=0)
             }
             
             if(nrow(x@event.pred.data)!=0 && show.predictions){
               abline(v=x@event.pred.data$time, lty="dashed", col="brown")
               abline(h=x@event.pred.data$event, lty="dashed", col="brown")
               points(x@event.pred.data$time, x@event.pred.data$event, cex=1.5, pch=21, col="red", bg="yellow")
             }
             if(nrow(x@time.pred.data)!=0 && show.predictions){
               abline(v=x@time.pred.data$time, lty="dashed", col="brown")
               abline(h=x@time.pred.data$event, lty="dashed", col="brown")
               points(x@time.pred.data$time, x@time.pred.data$event, cex=1.5, pch=21, col="red", bg="yellow")
             }
             
             #legend
             CItext <- paste("CI [",x@limit,",",1-x@limit,"]",sep="")
             leg.text <- c("Recruitment", "Predicted Events",CItext)
             leg.col <-  c("black","blue","red")
             leg.lty <-  c(1,1,8)
             
             if(nrow(eventdata)>0){
               leg.text <- c(leg.text,"Analysis Date")
               leg.lty <- c(leg.lty,3)
               leg.col <- c(leg.col,"blue")
             }
             
             if(show.predictions && (nrow(x@event.pred.data) > 0 || nrow(x@time.pred.data)>0) ){ 
               leg.text <- c(leg.text,"Predictions")
               leg.lty <- c(leg.lty,3)
               leg.col <- c(leg.col,"brown") 
             }   
             
             if(include.dropouts && x@dropout.rate!=0){
               leg.text <- c(leg.text,"Predicted Dropouts")
               leg.lty <- c(leg.lty,1)
               leg.col <- c(leg.col,"aquamarine4") 
             }
             
             if(include.dropouts && x@dropout.rate==0 && sum(x@event.data@subject.data$withdrawn)!=0 ){
               leg.text <- c(leg.text,"Dropouts")
               leg.lty <- c(leg.lty,1)
               leg.col <- c(leg.col,"tan") 
             }
             
             
             legend(legend.position,
                    leg.text,col=leg.col,lty=leg.lty,text.col=leg.col,
                    lwd=rep(2,length(leg.text)),bty="n")
             
             par(mar=oldmar)
             
})

