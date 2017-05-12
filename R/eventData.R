# This file contains the public functions associated with eventData object
# in the predict from data part of the package. Note the estimateAccrualParameter
# function can be found in the accrual.R file.

##' @include fromDataSimParam.R eventPrediction_package.R
NULL

##' @import survival methods
NULL

##' @importFrom  mvtnorm rmvnorm
NULL

##' @importFrom  scales date_format
NULL

# Function that will check the validity of EventData object
# when constructed
# @param object EventData object
checkEventData <- function(object){
  errors <- ""
  
  required.colnames <- c("has.event", "rand.date", "withdrawn", "subject", "time","censored.at.follow.up")
  
  data <- object@subject.data
  
  if(nrow(data)>0){
  
    if(any(!data$has.event %in% c(0,1))) errors <- paste(errors,"has.event must be 1 or 0" )
    if(any(!data$withdrawn %in% c(0,1))) errors <- paste(errors,"withdrawn must be 1 or 0" )
    if(any(!data$censored.at.follow.up %in% c(0,1))) errors <- paste(errors,"censored.at.follow.up must be 1 or 0" )

    if(any(!required.colnames %in% colnames(data))) errors <- paste(errors,"invalid column names")
  
    
    if(class(data$rand.date)!="Date") errors <- paste(errors,"Invalid rand.date")
    if(class(data$time)!="numeric") {
      errors <- paste(errors,"Times must be numeric")
    }
    else{
      if(all(data$time==0)) errors <- paste(errors,"time value incorrect all are 0!")  
      if(any(data$time < 0 | is.na(data$time))) {
        errors <- paste(errors,"subjects cannot have non-positive time on trial. Please check subjects ",
                      paste(data[data$time <= 0 | is.na(data$time),]$subject,collapse=", "))
      
      }
    }
  
    if(any(duplicated(data$subject))) errors <- paste(errors,"subject ID must be unique")
  
    if(all(data$has.event==0)){
      warning("No events have occurred - a model cannot be fit")
    }
    if(all(data$has.event==1)) errors <- paste(errors,"all events have occurred!")
  
    idx <- which(data$has.event==1& data$withdrawn==1)
    if(length(idx)){
      errors <- paste(errors,"subjects cannot be both withdrawn and have an event. Please check subjects ",
                    paste(data$subject[idx],collapse=", "))
    }
  
  }
  
  if(!is.numeric(object@followup)||length(object@followup)!= 1|| object@followup <= 0){
    errors <- paste(errors,"Invalid followup argument")
  }
  
  if(errors == "") TRUE else errors
}
  
##' Class representing data to use for new predictions
##' @slot subject.data a data frame with 6 columns
##' "subject", "rand.date", "has.event", "withdrawn", "censored.at.follow.up" and "time" and "site" and "subgroup" and "event.type"
##' see vignette and \code{EventData} for further details. 
##' @slot followup A numeric value for the fixed follow up period a subject
##' is followed. This is in days. If there is no fixed followup period then 
##' Inf should be used
##' @export
setClass("EventData", 
          slots= list(subject.data = "data.frame",
                      followup="numeric"),
          validity = checkEventData)


##' Constructor for event data object
##' 
##' All dates must be in one of the following formats:
##' YYYY-MM-DD, DD/MM/YY or DD Month YYYY
##'  
##' 
##' @param data A data frame 
##' @param subject string, the column name of subject identifiers
##' @param rand.date string, the column name of randomization dates
##' @param has.event string, the column name of whether a subject had an event
##' (1) or not (0) 
##' @param withdrawn string, the column name of whether a subject has withdrawn
##' (1) or not (0)  
##' @param time Either a string, the column name of time each subject has been on the study
##' or a list with elements with (some of) the following named elements.
##' \code{last.date}, \code{event.date}, \code{prog.date}, \code{dth.date} and \code{withdrawn.date}
##' In this case the package will attempt to derive the time column 
##' See the vignette and then eventPrediction:::AddTimeColumn for further details
##' @param site optional column for subject site
##' @param subgroup optional column for subgroup. Used by eventTools.
##' @param event.type optional column for the event type (e.g. unstable angina) if not included then `Had Event' will be used 
##' @param remove.0.time logical, if TRUE then all subjects with time = NA or 0 are removed from the
##' data set and not included in the object. If FALSE then they are included in the simulation (but not in the model fitting)
##' @param followup A numeric value for the fixed follow up period a subject
##' is followed. This is in days. If there is no fixed followup period then 
##' Inf should be used
##' @return An \code{EventData} object
##' @export
EventData <- function(data,subject,rand.date,has.event,withdrawn,time,site=NULL,subgroup=NULL,event.type=NULL,remove.0.time=FALSE,followup=Inf){
  
  #set time argument
  arg <- if(!is.list(time))  time else NULL
   
  #check columns are in data frame 
  for(x in c(subject,rand.date,has.event,withdrawn,arg)){
    if(!is.null(x) && !x %in% colnames(data)){
      stop(paste("Column name",x,"not found in data frame"))
    }
  }
  
  if(!is.numeric(followup) || length(followup)> 1 || followup <= 0){
    stop("Invalid followup argument")
  }
  
  #validate the input columns, deriving time from other columns if needed
  data[,rand.date] <- if(nrow(data)>0) FixDates(data[,rand.date]) else data[,rand.date]
  time <- if(!is.list(time))  data[,time] else AddTimeColumn(data,rand.date,has.event,withdrawn,subject,time)
  site <- if(is.null(site)) rep(NA,nrow(data)) else data[,site] 
  subgroup <- if(is.null(subgroup)) rep(NA,nrow(data)) else data[,subgroup] 
  
  if(is.null(event.type)) 
    event.type <- ifelse(data[,has.event],"Has Event",factor(NA))
  else{
    event.type <- factor(data[,event.type])
    if("" %in% levels(event.type)){
      event.type[event.type==""] <- factor(NA)
      event.type <- droplevels(event.type)
    }
  }
  
  #unvalidated data frame
  subject.data <- data.frame(
    subject=data[,subject],
    rand.date=data[,rand.date],
    time=time,
    has.event=data[,has.event],
    withdrawn=data[,withdrawn],
    site=site,
    subgroup=subgroup,
    event.type=event.type
  )
  
     
  keep <- which(as.character(subject.data$subject) > "" & !is.na(subject.data$rand.date))
  if(length(keep) != nrow(subject.data)){
    warning(paste("Subjects",paste(subject.data[setdiff(1:nrow(subject.data),keep),subject],collapse=", "),
                  "have been removed due to invalid rand.date or missing subject ID."))
  }
  
  subject.data <- subject.data[keep,]
  
  subject.data <- warnNAtime(remove.0.time,subject.data)
  
  subject.data <- IncludeFollowUp(subject.data,followup)
  
  #finally run some validation checks which change the input data and output warnings
  validation.checks <- function(subject.data,idx,warning.message,fn){
    if(length(idx)>0){
      warning(paste("Subjects",paste(subject.data[idx,subject],collapse=", "),warning.message))
      subject.data <- fn(subject.data,idx)
    }
    subject.data
  }
  
  subject.data <- validation.checks(subject.data,
                    idx=which(subject.data$has.event==1 & is.na(subject.data$event.type)),
                    warning.message="have an event and no event type, their event type is set to 'Had Event'",
                    fn=function(subject.data,idx){
                      if(!"Had Event" %in% levels(subject.data$event.type)){
                        levels(subject.data$event.type) <- c(levels(subject.data$event.type),"Had Event")
                      }
                      subject.data$event.type[idx] <- "Had Event"
                      return(subject.data)
                    })
  
  subject.data <- validation.checks(subject.data,
                    idx=which(subject.data$has.event!=1 & !is.na(subject.data$event.type)),
                    warning.message="have an event type but did not have an event",
                    fn=function(subject.data,idx){
                      subject.data$event.type[idx] <- factor(NA)
                      subject.data$event.type <- droplevels(subject.data$event.type)
                      return(subject.data)
                    })
  
  subject.data <- validation.checks(subject.data,
                    idx=which(subject.data$has.event==1& subject.data$withdrawn==1),
                    warning.message="have an event and are withdrawn, assuming they have an event at the given time",
                    fn=function(subject.data,idx){
                      subject.data$withdrawn[idx] <- 0
                      return(subject.data)
                    })

  #further validation occurs in the validity function of the class 
  return(new("EventData", subject.data = subject.data, followup=followup))
}



##' @name show
##' @rdname show-methods
##' @aliases show,EventData-method
##' @export
setMethod("show",
          "EventData",
    function(object) {
         cat("EventData, use object@subject.data$param to access individual columns:\n")
         cat(str(object@subject.data))
     })


##' @name summary
##' @rdname summary-methods
##' @aliases summary,EventData-method
##' @export
setMethod("summary","EventData",
  function(object){
    df <- object@subject.data 
    daysinyear <- standarddaysinyear()
    
    if(nrow(df)>0){
    
      cat(paste("Number of subjects:",nrow(df),"\n"))
      cat(paste("Number of events:",sum(df$has.event==1),"\n"))
    
      if(sum(df$has.event==1) != 0 && length(levels(df$event.type)) != 1){
        tab <- table(df$event.type)
        lapply(names(tab),function(y){cat(paste(" Of type ",y,": ",tab[y],"\n",sep=""))})
      }
    
      cat(paste("Number of withdrawn:", sum(df$withdrawn==1),"\n"))
      cat(paste("First subject randomized:",as.Date(min(df$rand.date),origin="1970-01-01"),"\n"))
      cat(paste("Last subject randomized:",as.Date(max(df$rand.date),origin="1970-01-01"),"\n"))
    
      if(sum(object@subject.data$has.event)>0){    
        cat(paste("First Event:",as.Date(min(LastDate(df[df$has.event==1,]),na.rm=TRUE),origin="1970-01-01"),"\n"))
        cat(paste("Last Event:",as.Date(max(LastDate(df[df$has.event==1,]),na.rm=TRUE),origin="1970-01-01"),"\n"))
      }
      
      av <- average.rec(nrow(df),as.numeric(min(df$rand.date)),as.numeric(max(df$rand.date))) 
      cat(paste("Average recruitment (subjects/day):",roundForceOutputZeros(av,2),"\n"))
    
      if(!is.infinite(object@followup)){
        cat(paste("Subjects followed for ",round(object@followup), " days (",
                round(object@followup/daysinyear,2)," years)\n",sep=""))
      
        cat(paste("Number of subjects censored at end of follow up period:",
                  sum(object@subject.data$censored.at.follow.up),"\n"))
      
      }
    }
    else{
      cat("Empty data frame!\n")
    }
})



##' Fit a survival model to an EventData object
##' @rdname fit-methods
##' @name fit
##' @param object an EventData object
##' @param ... Additional arguments to the function
##' @param dist character, either weibull or loglogistic, which model
##' is to be fit 
##' @return an EventModel object
##' @export
if (!isGeneric("fit")){
  setGeneric("fit", function(object,...) standardGeneric("fit"))
}



##' @name fit
##' @rdname fit-methods
##' @aliases fit,EventData-method
##' @export
setMethod("fit","EventData",function(object,dist="weibull"){
  
  if(!dist %in% c("weibull","loglogistic")){
    stop("dist must be weibull or loglogistic")
  }
  
  if(nrow(object@subject.data)==0)stop("Empty data frame!") 
  if(sum(object@subject.data$has.event)==0){
    stop("Cannot fit a model to a dataset with no events")
  }
  
  #subjects with time = 0 are set to NA for the model fitting
  #so they are ignored
  indat <- object@subject.data
  indat$time <- ifelse(indat$time==0,NA,indat$time)
  
  model<-survreg(Surv(time, has.event) ~ 1, data=indat, dist=dist, y = TRUE)
  
  new("EventModel",model=model,event.data=object,
      simParams=FromDataParam(object=model,type=dist))
})



##' @rdname plot-methods
##' @name plot
##' @param by.subgroup Plot separate lines per subgroup
##' @aliases plot,EventData,missing-method
##' @export
setMethod( "plot",
  signature( x="EventData", y="missing" ),
  function(x, xlab="log(t)", ylab="log(-log(S(t)))", 
           main="", by.subgroup = FALSE, ...) {
    
    if(nrow(x@subject.data)==0)stop("Empty data frame!")
    if(sum(x@subject.data$has.event)==0){
      stop("Cannot fit a model to a dataset with no events")
    }
    

    subgroups <- as.factor( x@subject.data$subgroup )
    subgr <- levels( subgroups )
    if( by.subgroup && !any( is.na(subgroups) ) && length( subgr )==2 ) {
      data.1 <- x@subject.data[ x@subject.data$subgroup==subgr[1], ]
      data.2 <- x@subject.data[ x@subject.data$subgroup==subgr[2], ]
      model.1 <- survfit(Surv(time, has.event) ~ 1, data=data.1,...)
      model.2 <- survfit(Surv(time, has.event) ~ 1, data=data.2,...)
      
      res.1 <- data.frame(t=model.1$time, s=model.1$surv, subgroup=subgr[1] )
      res.2 <- data.frame(t=model.2$time, s=model.2$surv, subgroup=subgr[2] )
      res <- rbind( res.1, res.2 )
      res <- res[res$t>0 & res$s>0 & res$s<1,]
      df <- data.frame(x=log(res$t),y=log(-log(res$s)),Subgroup=res$subgroup)
      p <- ggplot(df, aes_string(x="x", y="y", group="Subgroup", colour="Subgroup")) + 
        scale_colour_manual(values= c( "red", "blue" )) 
    } else {
      if( by.subgroup ){ warning( "Cannot plot by subgroup, please check that 2-levels and no NAs" )}
      model <- survfit(Surv(time, has.event) ~ 1, data=x@subject.data,...)
      res <- data.frame(t=model$time, s=model$surv)
      res <- res[res$t>0 & res$s>0 & res$s<1,]
      df <- data.frame(x=log(res$t),y=log(-log(res$s)))
      p <- ggplot(df, aes_string(x="x", y="y")) 
    }
    p + geom_point() +
      stat_smooth(method="lm", se=FALSE, color="red", size=1 ) +
      xlab(xlab) + ylab(ylab) +
      ggtitle(main) + theme_bw() +
      theme(panel.grid.minor = element_line(colour="gray", size=0.5))
  }
)



##' Method to create new \code{EventData} object only including recruitment times
##' @param object The  object from which to create the new \code{EventData} object
##' @rdname OnlyUseRecTimes-methods
##' @return An \code{EventData} object with time=0 and no events or withdrawn subjects
##' @name OnlyUseRecTimes
##' @export
setGeneric("OnlyUseRecTimes",function(object) standardGeneric("OnlyUseRecTimes"))

##' @rdname OnlyUseRecTimes-methods
##' @name OnlyUseRecTimes
##' @aliases OnlyUseRecTimes,EventData-method
##' @export
setMethod("OnlyUseRecTimes","EventData",function(object){
  object@subject.data$withdrawn <- 0
  object@subject.data$has.event <- 0
  object@subject.data$censored.at.follow.up <- 0
  object@subject.data$time <- 0
  object@subject.data$event.type <- factor(NA)
  object@subject.data$event.type <- droplevels(object@subject.data$event.type)
  object
})


##' Cut the Event Data
##' 
##' Create a new EventData from an existing one which shows how the
##' data would have looked on a given date (assuming no lag in reporting of 
##' events and if necessary subjects have been censored at cutoff point).
##' If subject was censored before the cut date then their censored date remains
##' unchanged
##' @param object The EventData object
##' @param date The date to cut the data at
##' @rdname CutData-methods
##' @name CutData
##' @return \code{EventData} object with the data censored at the appropriate point
##' @export
setGeneric("CutData",function(object,date) standardGeneric("CutData"))


##' @rdname CutData-methods
##' @name CutData
##' @aliases CutData,EventData-method
##' @export
setMethod("CutData","EventData",function(object,date){

  cut.date <- FixDates(date)
  
  subject.data <- object@subject.data
  
  #only include subjects who have been randomized by this time
  subject.data <- subject.data[subject.data$rand.date<=cut.date,]
  if(nrow(subject.data)==0){
    stop("Cut date before first subject randomization")
  }
  
  censored.times <- cut.date - subject.data$rand.date + 1
  idx <- which(censored.times < subject.data$time)
  
  if(length(idx)==0&&nrow(subject.data)==nrow(object@subject.data)){
    warning("Cut date is too late (no ongoing subjects at this time) and so has no effect")
  }
  
  
  
  subject.data$time <- pmin(censored.times,subject.data$time)
  subject.data$has.event[idx] <- 0
  subject.data$event.type[idx] <- factor(NA)
  subject.data$event.type <- droplevels(subject.data$event.type)
  subject.data$withdrawn[idx] <- 0
  
  EventData(
    data=subject.data,
    subject="subject",
    rand.date="rand.date",
    has.event="has.event",
    withdrawn="withdrawn",
    time="time",
    site="site",
    subgroup="subgroup",
    event.type="event.type",
    followup=object@followup
  )
  
})

##' This function calculates the event type for pfs data
##' 
##' This function is called before creating the EventData object 
##' @param has.event A vector or whether subjects had event 
##' @param prog.date A vector of dates (or strings in the standard eventPrediction allowed formats)
##' of progression.date or blank if unknown 
##' @param dth.date A vector of dates (or strings in the standard eventPrediction allowed formats)
##' of death date or blank if unknown
##' @return A vector with NA if subject does not have event, 
##' "Death" if dth.date <= prog.date or prog.date blank,
##' "Progression (not death)" if prog.date < dth.date or dth.date blank
##' "Progression (unknown if death)" if both prog.date and dth.date are blank
##' @export
CalculateProgEventTypes <- function(has.event,prog.date,dth.date){
  #called before creating EventDate object so extra validation needed  
  
  if(any(!has.event %in% c(0,1))){
    stop("Invalid has.event argument")
  }
  
  prog.date <- FixDates(prog.date)
  dth.date <- FixDates(dth.date)
  
  if(!(length(prog.date)==length(dth.date) && length(prog.date) ==length(has.event))){
    stop("Invalid lengths of arguments")
  }
  
  unlist(lapply(seq_along(has.event),function(x){
    if(!has.event[x]) return(NA)
    
    dth <- dth.date[x]
    prog <- prog.date[x]
    
    if(!is.na(prog) && !is.na(dth)){
      if(dth<=prog) return("Death")  
      return("Progression (not death)")
    }  
    
    
    #just dth return dth
    if(!is.na(dth)) return("Death")
    
    #just prog return prog
    if(!is.na(prog)) return("Progression (not death)")
    
    #Otherwise are relying on lastDate so do not know which
    return("Progression (unknown if death)")
    
  }))
  
  
}


##' Create an \code{EventData} object with no subjects
##' 
##' @param followup The time subjects are followed until censoring  
##' @export
EmptyEventData <- function(followup=Inf){
  
  if(length(followup) > 1 || !is.numeric(followup) || followup < 0){
    stop("Invalid followup argument")
  }
  
  d <- data.frame(subject=character(0),
                  randDate=numeric(0),
                  has.event=numeric(0),
                  withdrawn=numeric(0),
                  time=numeric(0))
  
  #convert to an empty EventData object
   EventData(data=d,
             subject="subject",
             rand.date="randDate",
             has.event="has.event",
             withdrawn="withdrawn",
             time="time",
             followup=followup)
}

##' Calculate the number of days at risk
##' 
##' For \code{EventData} object this is the number of days
##' at risk of the input data. For \code{SingleSimDetails} object
##' this is the median numberof days at risk in the simulation set at the given time
##' 
##' @param object The object to calculate the days at risk  
##' @param ... Additional arguments passed to the function
##' @param times A vector of dates for calculating cutting the simulated date at in order to
##' calculate the number of days at risk by this point.
##' @name CalculateDaysAtRisk
##' @rdname CalculateDaysAtRisk-methods
##' @export
setGeneric("CalculateDaysAtRisk",function(object,...)standardGeneric("CalculateDaysAtRisk"))


##' @rdname CalculateDaysAtRisk-methods
##' @name CalculateDaysAtRisk
##' @aliases CalculateDaysAtRisk,EventData-method
##' @export
setMethod("CalculateDaysAtRisk","EventData",
          function(object){
            return(sum(object@subject.data$time))            
          }
)

##' Makes a barplot with the number of events over time
##' 
##' @param object The object to create the event versus time plot 
##' @param ... Additional arguments passed to the function
##' @param timeunit The resoloution on the x-axis (month, weeks or quarter)
##' @name EventVsTimePlot
##' @rdname EventVsTimePlot-methods
##' @export
setGeneric("EventVsTimePlot",function(object,timeunit="Months",...)standardGeneric("EventVsTimePlot"))


##' @rdname EventVsTimePlot-methods
##' @name EventVsTimePlot
##' @aliases EventVsTimePlot,EventData-method
##' @export
setMethod("EventVsTimePlot","EventData",
          function( object, timeunit="month" ){
            my.data <- object@subject.data
            my.data$lastdate <- LastDate( my.data )
            my.data$has.event <- as.integer( my.data$has.event )
            my.data$mytimeunit <- as.Date( cut( my.data$lastdate, 
                                                breaks = timeunit ) )
            
            mybreaks <- if( timeunit == "quarter" ) "3 months" else "1 month" 
            
            ggplot( data = my.data, aes_string( x="mytimeunit", y="has.event" )) +
              stat_summary( fun.y = sum, geom = "bar") + 
              scale_x_date(
                date_labels = "%Y-%b",
                date_breaks = mybreaks ) + 
              theme_bw() + 
              theme( axis.text.x = element_text(angle = 45, hjust = 1) ) +
              xlab( "" ) +
              ylab( "Observed events" )
          }
)

