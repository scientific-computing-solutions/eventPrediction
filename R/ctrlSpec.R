#This file contains the code required for the
#CtrlSpec class used in predict from parameters
#It is not exported for users
#and is used to translate the event/drop out rate 
#specifications in the Study constructors to a common 
#form to be stored within the Study object
#it is also used for the dropout specification in predict from
#data

# The CtrlSpec (control arm specification) class
# is not exported for user use but is used within the
# package to allow different ways of specifying the
# contorl arm rate when specifying a Study/CRGI study
# It also includes the text to be output in the summary text 
# concerning the control (and experimental) arm event rates.
# This class is also used when specifying the drop out rate for the 
# control arm
# @slot median The control arm median event time
# @slot text The control arm text to be included in the summary
# @slot experimentaltext a function which takes a HR and shape and outputs 
# the summary text information for the rate spec of the control arm    
setClass( "CtrlSpec", 
          slots= list(median="numeric",  
                      text="character", 
                      experimentaltext="function"
                      )
)


# Create a \code{CtrlSpec} by giving the control arm median survival
# time as used in the Oncology study constructor
# @param ctrl.median The control arm median time to event
# @return A \code{CtrlSpec} object
CtrlSpecfromMedian <- function(ctrl.median){

  if(!is.na(ctrl.median)){
    if(!is.numeric(ctrl.median) || length(ctrl.median) > 1 || ctrl.median < 0){
     stop("Invalid ctrl.median argument, it must be numeric and positive")
    }
  }
  
  f <- function(HR,shape){
    paste("median=",round(ctrl.median/HR^(1/shape),2),sep="")
  }
  
  new("CtrlSpec",median=ctrl.median,text=paste("median=",ctrl.median,sep=""),
      experimentaltext=f)  
    
}


# Create A CtrlSpec object from a list
# 
# The function calls \code{CtrlSpecfromProportion} with the appropriate
# arguments taken from a list. It is used when creating the dropout slot of 
# the \code{Study} object in the various Study constructors
#
# @param eventtext The text to be output in the summary of the ctrlspec object
# for example either "had an event by" or "(in absence of events) would drop out by"
# @param var.list A list with elements labelled time and proportion
# (and optionally shape). Or NULL for creating a CtrlSpec object with infinite median
# (i.e. specifying the median for drop outs given no drop outs)
# @param n.arms The number of arms for which to create CtrlSpec objects  
# @return A list of CtrlSpec objects one for each arm
CtrlSpecFromList <- function(var.list,eventtext,n.arms){
  
  #Infinite median if no "events" (used when specifying the median there are no drop outs)
  if(is.null(var.list)){
    x <- CtrlSpecfromProportion(time=Inf,proportion.had.event=0.5,shape=1)
    return(if(n.arms==1) list(ctrl=x) else list(ctrl=x,active=x))
  }
  
  if(!is.list(var.list) || is.null(var.list$time) || is.null(var.list$proportion) || length(var.list$proportion) > 2
     || length(var.list$proportion)!= n.arms){
    stop(paste("Invalid argument(s) to CtrlSpecFromList.",
               "See help for the dropout argument to Study function",
               "for an example of valid arguments."))
  }
  
  if(is.null(var.list$shape)) var.list$shape <- 1
  
  retVal <- lapply(var.list$proportion,function(x){
    CtrlSpecfromProportion(var.list$time,x,var.list$shape,eventtext)
  })
  
  names(retVal) <- if(n.arms==1) "ctrl" else c("ctrl","active")
  return(retVal)
  
}

# Create a \code{CtrlSpec} object giving the time at which a given 
# proportion of subjects have had an event as used in the CRGIStudy constructor
# @param time The time at which \code{ctrl.proportion} of the control group have had an event.
# Used to set the event rate 
# @param proportion.had.event The proportion of control arm subjects who have had an event by time \code{time} 
# @param shape The Weibull shape parameter for the control arm survival function
# @param eventtext The text to be output in the summary of the ctrlspec object
# for example either "had an event by" or "(in absence of events) would drop out by"
# @return A \code{CtrlSpec} object
CtrlSpecfromProportion <- function(time,proportion.had.event,shape,eventtext="had an event by"){
  
  lapply(list(shape=shape,time=time,proportion.had.event=proportion.had.event),
    function(x){
      if(!is.numeric(x) ||  length(x) != 1 || x <= 0){
       stop("Arguments to CtrlSpecfromProportion should be numeric, positive and of length 1 ") 
      }
    }       
  )
  
  if(proportion.had.event >= 1){
    stop("Invalid proportion.had.event argument")
  }
      
  text <- paste(proportion.had.event*100,"% of subjects ",eventtext," ",time,sep="")
  rate <- ((-log(1-proportion.had.event))^(1/shape))/time
  
  median <- log(2)^(1/shape)/rate
  
  f <- function(HR,shape){
    paste(round( (1-(1-proportion.had.event)^HR)*100, 2),"% of subjects ",eventtext," ",time,sep="")
  }
  
  new("CtrlSpec",median=median,text=text,experimentaltext=f)
  
}