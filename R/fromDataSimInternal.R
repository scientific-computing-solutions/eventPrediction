#This file contains the private functions
#for the predict from data "simulate" part of the package

# Validate the arguments to the simulate procedure
# @param accrualGenerator An AccrualGenerator object used for recruiting additional subjects 
# @param Naccrual The number of additional subjects to be recruited
# @param Nsim Number of simulations to run 
# @param limit Limit for the percentiles, default is 0.05 which corresponds
# to [0.05, 0.95]
# @param seed Integer for random number generator (for reproducability).
# @param longlagsettings A LongLagSettings object to control the behaviour of the algorithm for subjects whos last date
# is a long time from the analysis date - see vignette for further details
# @param HR The hazard ratio: an advanced option which allows two arm trials to be simulated. This replicates the 
# Predict from parameters functionality but uses the recruitment times found in \code{data}. See the vignette for
# further details    
# @param r The allocation ratio: see \code{HR} argument.
# @param data The EventData object for the simulation
validate.simulate.arguments <-function(accrualGenerator,Naccrual,Nsim,seed,limit,
                                       longlagsettings,HR,r,data){
  
  if(xor(is.null(HR),is.null(r)))
    stop("It is not possible for only one of HR and r to be NULL")
  
  if(!is.null(HR)){
    if(!is.numeric(HR) || HR <= 0 || length(HR) != 1){
      stop("Invalid HR")
    }
  }
  
  if(!is.null(r)){
    if(!is.numeric(r) || r <= 0 || length(r) != 1){
      stop("Invalid r")
    }
  }
  
  if(!is.null(HR) && ( nrow(data@subject.data)!= 0 && any(!is.na(data@subject.data$time) & data@subject.data$time != 0 ))){
    stop("If the argument HR is used then the time on the study for each subject must be 0
         (only recruitment times are used when this option is used)")
  }
  
  
  
    
  if(!is.numeric(limit) || limit < 0 || limit > 0.5 || length(limit) > 1)
    stop("invalid limit argument")
  
  if(!is.null(longlagsettings) && class(longlagsettings)!= "LongLagSettings"){
    stop("longlagsettings must be of class LongLagSettings")
  }
  
  if(Naccrual < 0 || length(Naccrual)>1) stop("invalid Naccrual argument")
  
  if(Naccrual==0){
    if(!is.null(accrualGenerator)) warning("using an accrual generator but Naccrual = 0")
  }
  else{
    if(is.null(accrualGenerator) || class(accrualGenerator)!="AccrualGenerator"){
      stop("accrual generator must be an AccrualGenerator object if Naccrual > 0")
    }
  }
  
  if(!is.null(seed) && !is.numeric(seed)) warning("invalid seed")
  
  if(!is.numeric(Nsim) || Nsim < 0) stop("invalid Nsim argument")
  if(Naccrual+nrow(data@subject.data)==0) stop("No subjects!")
}


# Generate which subjects are to have HR=1 (i.e. to be on the control arm)
# and which are to be on the active arm
# @param HR The hazard ratio
# @param r The allocation ratio
# @param N The number of subjects whose events are being simulated
# @return A vector of length N containing 2 different numbers: The given 
# hazard ratio and the number 1 (for subjects on control arm)
# The number of each element is set by \code{r}
# and the elements are randomly permuted       
GetHRs <- function(HR,r,N){
  Ns <- getNs(singleArm=FALSE,r,N)
  HRs <- c(rep(1,Ns[1]),rep(HR,Ns[2]))
  sample(HRs,size=N,replace=FALSE)
}


# Accrual times for the existing and simulated subjects
# @inheritParams validate.simulate.arguments
# @param rand.dates The dates existing subjects have been randomized
# @return A (unsorted) matrix of recruitment dates as numerics (1 row per simulated trial)
# for subjects 
CalculateAccrualTimes <- function(Naccrual,Nsim,rand.dates,accrualGenerator){
  rs <-t(replicate(Nsim,rand.dates,simplify = "matrix"))
  if(length(rand.dates)==1) rs <- matrix(rs,nrow=Nsim)
  
  if(Naccrual==0){
    return(rs)
  }
  
  newrecs <- if(Naccrual==1) matrix(replicate(Nsim, accrualGenerator@f(Naccrual)),ncol=1)
             else t(replicate(Nsim, accrualGenerator@f(Naccrual)))
      
  if(length(rand.dates)==0) return(newrecs)
  
  if(min(newrecs) < max(rand.dates)){
    warning("Some new recruited subjects have rand.date earlier than some existing subjects")
  }
  
  return(cbind(rs,newrecs))
}
  

# Output Inf Date
#
# @return A Inf Date
WithdrawnEventDate <- function(){
  as.Date(Inf,origin="1970-01-01")
}



# Function to perform a single simulation
# 
# @param row A vector containing 3 elements first the rate for events hazard, 
# second the shape for the event hazard and third the index of the simulation
# @param number.subjects The number of existing subjects in the data frame
# @param Naccrual The number of additional subjects recruited
# @param indat The subject.data slot of the EventData object (after the long lagsettings have been applied)
# @param newrecs A matrix of recruitment times newrecs[row[[3]],] is a vector of recruitment times
# needed for simulated subjects for the single simulation
# @param HR The hazard ratio: an advanced option which allows two arm trials to be simulated. This replicates the 
# Predict from parameters functionality but uses the recruitment times found in \code{data}. See the vignette for
# further details    
# @param r The allocation ratio: see \code{HR} argument.
# @param dropout.rate Weibull rate parameter for subject drop out
# @param dropout.shape Weibull shape parameter for subject drop out
# @param followup The subject follow up time (Inf if followed until event/withdrawn) 
# @param conditionalFunction The conditionalFunction slot of a \code{FromDataSimParam} object
# @return A list with two elements both vectors of length 
# Naccrual+number.subjects. event.type which contains 0 if event occurs
# 1 if subject dropped out or 2 if subject completed follow up period
# event.times the dates (as numeric) of subjects leaving the trial due to follow up
PerformOneSimulation <- function(row,number.subjects,Naccrual,
                                 indat, newrecs,HR,r, dropout.rate, 
                                 dropout.shape, followup,conditionalFunction){
  #first set all subjects to having an event
  #0 = event, 1 = dropout, 2 = follow up
  event.type <- rep(0,number.subjects+Naccrual)
  
  #And the structure for the time of leaving the study
  retVal <- structure(rep(NA_real_, number.subjects+Naccrual), class="Date")
  
  #subjects who have already left the trial
  existing.events.index=which(indat$has.event==1 | indat$withdrawn==1 | indat$censored.at.follow.up==1)
  
  #set their date and event type correct
  retVal[existing.events.index] <- LastDate(indat)[existing.events.index]
  event.type[existing.events.index] <- ifelse(indat$has.event[existing.events.index]==1, 0,
                                              ifelse(indat$censored.at.follow.up[existing.events.index]==1,2,1))
  
  #get subjects who have not had an event/withdrawn
  w <- which(indat$has.event==0 & indat$withdrawn==0 & indat$censored.at.follow.up==0)
  lower.bounds <- indat$time[w]
  rand.date <- indat$rand.date[w]
  
  #add to them accrued subjects
  if(Naccrual > 0){
    lower.bounds <- c(lower.bounds,rep(0,Naccrual))
    rand.date <- if(Naccrual > 1) c(rand.date,as.Date(newrecs[row[names(row)=="Id"],],origin="1970-01-01"))
                 else c(rand.date,as.Date(newrecs[row[names(row)=="Id"]],origin="1970-01-01"))
    w <- c(w,(number.subjects+1):(number.subjects+Naccrual))
  }
  
  #create a vector of HRs 1 for each subject. If subject is on control arm
  #(or HR is null) then HR=1 otherwise HR is the given HR
  HRs <- if(is.null(HR)) rep(1,length(lower.bounds)) else GetHRs(HR,r,length(lower.bounds))
  
  #generate the time on trial
  times <- conditionalFunction(t.conditional=lower.bounds,params=row,HR=HRs) 
  
  #if competing risks dropout then deal with this
  if(dropout.rate != 0){
    dropout.times <- rcweibull(lower.bounds,params=list(shape=dropout.shape,rate=dropout.rate),
                               HR=rep(1,length(lower.bounds)))
    event.type[w] <- ifelse(times < dropout.times,0,1)
    times <- pmin(times,dropout.times)
  }
  
  #if anytimes are > followup then set them as followup
  lost.to.followup <- which(times > followup)
  times[lost.to.followup] <- followup
  event.type[w[lost.to.followup]] <- 2
  
  #and the event times
  tmp <- internal.Date(pmax(1,round(times)),rand.date)                
  
  
  #put the times into the return vector
  retVal[w] <- tmp
  
  return(list(event.type=event.type,
              event.times=retVal))
  
}
