# The predict from parameters non-exported functions

# Calculate the expected number of event occuring at the given
# times
# 
# @param times A vector of times, see the time.pred argument
# to the S4 predict method for the Study object
# @param study A \code{Study} object
# @param sfns The survival functions the output from \code{GetSurvivalFunctions}
# @param calc.at.risk Logical, if true calculate the time at risk 
# @return A data frame containing the required data. See AnalysisResults
# slot grid for more details
CalculateEventsGivenTimes <- function(times,study,sfns,calc.at.risk=TRUE){
  #Sort out recruitment
  m         <- pmin( times, study@acc.period )
  rec.rate  <- m^study@k/study@acc.period^study@k
  Ns <- getNs(isSingleArm(study),study@r,study@N)
  recruit     <- rec.rate %*% t( Ns )
  recruit.tot <- apply( recruit, 1, sum )

  #Calculate events
  eventscdf <- lapply(sfns,function(x){events.integ(x,study@acc.period, study@k, times)})
  events <-  cbind( eventscdf[[1]]*Ns[ 1 ], eventscdf[[2]]*Ns[ 2 ] )
  events.tot <- apply( events, 1, sum )
  rounded.events.tot <- floor(events[,1])+floor(events[,2])
  
  df <- data.frame(time=times,events1=events[,1],events2=events[,2],events.tot=events.tot,recruit.tot=recruit.tot,
             rounded.events.tot,time.pred=rep(TRUE,length(times)))
  
  if(calc.at.risk){
    atriskcdf <- lapply(sfns,function(x){atrisk.integ(x,study@acc.period, study@k, times)})
    atrisk <- cbind( atriskcdf[[1]]*Ns[ 1 ], atriskcdf[[2]]*Ns[ 2 ] )
    atrisk.tot <- apply(atrisk,1,sum)
    df <- cbind(df,at.risk1=atrisk[,1],at.risk2=atrisk[,2],atrisk.tot=atrisk.tot)
  }
  
  return(df)
  
  
  
}

# Calculate the expected time a given number of events 
# will have occured
# 
# @param event.pred A vector of numbers of events, see the event.pred argument
# to the S4 predict method for the Study object
# @param study A \code{Study} object
# @param sfns The survival functions the output from \code{GetSurvivalFunctions}
# @param grid The output to \code{CalculateEventsGivenTimes} which is used
# to find good starting points for the search for thr required times 
# @param supress.warning logical, should the warning about too many events be supressed
# @return A data frame containing the required data. See AnalysisResults
# slot grid for more details
CalculateTimesGivenEvents <- function(event.pred,study,sfns,grid,supress.warning=FALSE){
  roundedEvents <- grid$rounded.events.tot
     
  startIndex <- sapply(event.pred,function(x) sum(roundedEvents < x))
  
  ans.times <- unlist(lapply(1:length(event.pred),function(x){
    
    if(startIndex[x]==nrow(grid)){
      if(!supress.warning) warning("event.pred argument too large -- too many events for study.duration")
      NA
    }
    else{
      exact_time <- subdivide(grid[startIndex[x],"time"],
                           grid[startIndex[x]+1,"time"],
                           event.pred[x],study,sfns)
    }
  }))
  
  if(all(is.na(ans.times))){
    return(data.frame())
  }
    
  retVal <- CalculateEventsGivenTimes(ans.times[!is.na(ans.times)],study,sfns)
  retVal$time.pred<-rep(FALSE,nrow(retVal))
  retVal
}


# Find the exact time at which a requested number
# of events occurs
# 
# @param low A time at which the target number of events 
# has not yet occured
# @param high A time at which the target number of events
# has already occured
# @param aim The target number of events
# @param study The study 
# @param sfns list of Survival functions
# @return The time when the requested number of events occurs
subdivide <- function(low,high,aim,study,sfns){
  while(!isTRUE(all.equal(low,high))){
    
    mid <- (low+high)/2
    ans <- CalculateEventsGivenTimes(mid,study,sfns,calc.at.risk=FALSE)$rounded.events.tot
    
    if(ans >= aim){
      high <- mid
    }
    else{
      low <- mid
    }
    
  }
  return(high)
}

# Output a list of survival functions to be in the intergration
# to calculate the number of events -- one surival function per arm
# 
# If the study has a single arm then the second entry in the list is 
# the logical 'FALSE'
# 
# @param lambda A vector of rate parameters 
# for the arms of the study
# @param lambdaot A vector of rate parameters for time < T
# for studies with a lag
# @param lag.T The amount of lag for the study (=0 if no lag)
# @param isSingleArm Logical, True is trial has a single arm
# @param shape Weibull shape parameter of the study
# @param followup The time subjects are followed from randomization (=\code{Inf} if followed
# until end of study/event occurs) - this is only used if \code{lag.T} is zero.
# @param dropout.shape The Weibull shape parameter for the drop out hazard function 
# @param dropout.lambda The rate parameter for the drop out hazard function = 0 if no dropout 
# if no drop out 
# @return The survival functions
GetSurvivalFunctions <- function(lambda,lambdaot,lag.T,isSingleArm,shape,followup,dropout.shape,dropout.lambda){
  .range <- if(isSingleArm) 1 else 1:2
  ans <- lapply(.range,function(x){
    Sfn(lambda[x],lambdaot[x],lag.T,shape,followup,dropout.shape=dropout.shape,dropout.lambda=dropout.lambda[x])
  })
  
  if(isSingleArm){
    ans <- c(ans,NullSfn())
  }
  ans
}



# Calculate the required number of events to reject
# hypothesis H0: ln(HR)=0
# 
# An error is produced if more events are required than
# there are subjects on the trial. 
# See the vignette for details of the formula used.
# 
# @param r Allocation ratio 1:r, control:experiment
# @param alpha Statistical significance (assuming if a two sided 
# test then alpha has already been divided by 2)
# @param power Power of test
# @param HR Hazard ratio
# @param N total number of subjects recruited on trial
# @return The required number of events
RequiredEvents <- function(r,alpha,power,HR,N){

  events.req <- ((r + 1)*(qnorm(1 - alpha) + qnorm(power))) / 
                              (sqrt(r)*log(HR))
  
  events.req <- events.req*events.req
   
  if(events.req > N){ 
    warning(paste0("Given these settings, the (rounded) required number of events is ", round(events.req), 
                 " which is more than there are patients, please increase trial size!"))
  } 
  
  events.req  
}

# Calculate the events required for the study parameters
# the expected time for this to occur 
# and the critical HR at this number of events
#
# @param study A \code{Study} object
# @param sfns The survival functions the output from \code{GetSurvivalFunctions}
# @param grid The output to \code{CalculateEventsGivenTimes} which is used
# to find good starting points for the search for thr required times 
# @param av.HR The HR used to determine the number of events required
# @return A list containing \code{chr} the critical hazard ratio, 
# \code{critical.events.req}, the critical number of events required and
# \code{critical.data}, a data frame for the critical number of events (see grid
# slot for AnalysisResults). If single arm study then an empty data frame and 
# NA's for the other elements of the list is returned
CalculateCriticalValue <- function(study,sfns,grid,av.HR){
  
  if(isSingleArm(study)){
    chr<-as.numeric(NA)
    critical.data<-data.frame()
    events.req <- as.numeric(NA)
  }
  else{
    #Calculate the number of events required 
    alpha <-if(study@two.sided) study@alpha/2 else study@alpha
    events.req <- RequiredEvents(study@r,alpha,study@power,av.HR, study@N)
  
    ####### calculation of critical value - HR with 50% power ######
    chr <- exp(-1*((study@r + 1)*(qnorm(1 - alpha)+qnorm(0.5)))/sqrt(study@r*events.req))
    
    critical.data <- CalculateTimesGivenEvents(events.req,study,sfns,grid,supress.warning=TRUE)
  
  }
  return(list(chr=chr,critical.data=critical.data,
              critical.events.req=events.req))
  
}


# Function which calculates the average HR for a lagged study
# 
# See the vignette for details as to how this average is calculated
# 
# @param sfns A list of the survival functions
# @param study A Study object 
# @param Lagt The lag for the study
# @param LagHR The HR for time < t
# @param lambdaot The lambda for time < lag
# @param lambdatts The lambda for time > lag
# @param shape The Weibull shape parameter for the study
# @return The average HR
calculateAverageHR <- function(sfns,study,Lagt,LagHR,lambdaot,lambdatts,shape){
  
  
   
  sfnsLagt <- GetSurvivalFunctions(lambda=lambdaot,lambdaot=0,lag.T=0,
                                  isSingleArm=isSingleArm(study),
                                   shape=shape,followup=Lagt,study@dropout.shape,
                                   GetDropoutLambda(study))
   
  pot <- unlist(lapply(sfnsLagt,function(x)
         events.integ(x,study@acc.period, study@k,study@study.duration)))
  
  
  ptts <- if(Lagt < study@study.duration){  
            unlist(lapply(sfns,function(x) 
            events.integ(x,study@acc.period, study@k, study@study.duration))) - pot}
          else 
            c(0,0)
  
  
  Ns <- getNs(isSingleArm(study),study@r,study@N)
  w1 <- sum(pot*Ns)
  w2 <- sum(ptts*Ns)
  return(exp((w1*log(LagHR) + w2*log(study@HR))/(w1+w2)))
 
}


# Function that converts HR and control median into lambda
# @param median Control median
# @param hr Hazard ratio
# @param shape A Weibull Shape parameter
# @return A vector of hazard ratios
lambda.calc <- function( median, hr,shape){
  if(length(median)!=1 || length(shape)!=1 || (!is.na(hr) && length(hr) != 1)){
    stop("Invalid arguments to lambda.calc")
  }
  log(2)^(1/shape)/(median/c(1, hr^(1/shape)))
}


# Check the arguments to predict are valid
#
# @param time.pred A vector of times for which the number events are to be predicted
# @param event.pred A vector of events for which the times they are expected 
# to occur are output
# @param step.size The time between consecutive time points used for plotting the graphs
# @param study.duration the duration of the study
# @return Function throws an exception if any of the arguments are invalid
validatePredictArguments <- function(time.pred,event.pred,step.size,study.duration){

  if(!is.null(time.pred)){
    if(!is.numeric(time.pred) || any(time.pred < 0) || any(time.pred > study.duration) ){
      stop("Invalid time.pred argument")
    }
  }
  
  if(!is.null(event.pred)){
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
    if(!is.numeric(event.pred) || any(event.pred < 0) || any(!is.wholenumber(event.pred))){
      stop("Invalid event.pred argument")
    }
  
  }
  
  if(!is.numeric(step.size) || length(step.size) > 1 || step.size <= 0 || step.size > study.duration ){
    stop("Invalid step.size argument")
  }
  
}

# Return the drop out rates vector from a Study object
# @param study A Study object
# @return A vector of dropout rates - control then active for 2 arm studies, just control 
# arm for single arm studies 
GetDropoutLambda <- function(study){
  .range <- if(isSingleArm(study)) 1 else 1:2
  unlist(lapply(.range,function(x){
    lambda.calc(study@dropout[[x]]@median,1,study@dropout.shape)[1]  
  }))
}  


# Output the text concerning dropouts for the show method
# for the subject object
# 
# @param subject.text string denonting who the dropouts concern
# (e.g. "Subject" or "Control arm")
# @param dropoutspec The CtrlSpec object representing the dropout data to be output 
# @param dropoutshape If not Null then output the dropout shape
outputdropouttext <- function(subject.text,dropoutspec,dropoutshape=NULL){
  if(!is.infinite(dropoutspec@median)){
    cat(paste(subject.text,"drop out:",dropoutspec@text,"\n"))
    if(!is.null(dropoutshape)){
      if(dropoutshape==1){
        cat("Exponential drop out\n")
      }
      else{
        cat(paste("Weibull drop out with shape parameter",dropoutshape,"\n"))  
      }
    }
  }
  else{
    cat(paste(subject.text,"do not drop out","\n")) 
  }
}
