#This file contains the private functions
#for the predict from data part of the package
#apart from those used within the simulate function
#see fromDataSimInternal.R. For functions called by 
#AddTimeColumn see timeInternal.R

##'@include common.R simQOutput.R timeInternal.R
NULL


# Deal with subjects who have time = 0 or NA when creating 
# the EventData object
# 
# @param remove.0.time logical if TRUE then all subjects with time = NA or 0 are removed from the
# data set and not included in the object. If FALSE then they are included in the simulation 
# (but not in the model fitting)
# @param subject.data A data frame to be included in the subject.data slot 
# of the EventData object
# @return subject.data with appropriate subjects removed/time set to 0
warnNAtime <- function(remove.0.time,subject.data){
  
  if(!is.logical(remove.0.time) || length(remove.0.time) > 1){
    stop("Invalid remove.0.time argument")
  }
  
  index <- which(subject.data$time==0 | is.na(subject.data$time))
  if(length(index)==0){
    return(subject.data)
  }
  
  
  if(remove.0.time){
    warning(paste("Subjects",paste(subject.data[index,]$subject,collapse=", "),"have time=NA or 0",
                  "and have been removed from the data set."))
    subject.data <- subject.data[!1:nrow(subject.data) %in% index,] 
  }
  else{
    warning(paste("Subjects", paste(subject.data[index,]$subject, collapse=", "),
                  "have time=NA or 0, their time has been set to 0 and",
                  "they will not be used when fitting the model but will have event times simulated",
                  "when performing the predictions unless withdrawn is set to 1."))
    subject.data$time[is.na(subject.data$time)] <- 0
  }
  
  subject.data
}

# Get the multiplicative factor to convert units to
# days for the KM-plot
# @param units "Days", "Months" or "Years"
# @param daysinyear The number of days in a year (e.g. 365 or 365.25)
# @return 1, daysinyear/12 or daysinyear respectively
GetScaleForKM <- function(units,daysinyear){
  if(!units %in% c("Days","Months","Years")){
    stop("Invalid units argument")
  }
  
  xscale <- 1
  if(units == "Months") xscale <- daysinyear/12
  if(units == "Years") xscale <- daysinyear
  
  return(xscale)
}

# Function which inputs a vector of dates/strings
# and outputs a vector of dates
# 
# If S3 dates are input they are returned unchanged.
# If character strings are input then they must all
# have the same format either "YYYY-MM-DD", "DD/MM/YYYY"
# or "DD Month YYYY"
# 
# @param vec The vector of characters
# @return a vector of Dates.
# If conversion fails an error is thrown
FixDates <- function(vec){
     
   if(all(is.na(vec))) return(as.Date(vec))
   if(class(vec)=="Date") return(vec)
   
   vec[vec==""] <- NA
   formats <- c("%d/%m/%Y","%d %b %Y","%Y-%m-%d")
       
   ans <- lapply(formats,function(x){
     as.Date(vec,format=x)
   })
   
   which_date <- unlist(lapply(1:3,function(x){
     sum(is.na(ans[[x]]))==sum(is.na(vec))
   }))
   
   if(sum(which_date)!= 1){
     stop("Error reading date format it should be of the form
        YYYY-MM-DD, DD/MM/YYYY or DD Month YYYY ") 
   }
   
  ans <- ans[[which(which_date,TRUE)]]
  years <- as.character((ans),format="%Y") 
  years <- years[!is.na(years)]
   
  if(any(as.numeric(years) < 1000) ){
    stop("All years must have 4 digits, e.g. 01/01/2015 not 01/01/15")
  } 
   
  return(ans) 
}


# Set the censored.at.follow.up column for EventData@@subject.data
# @param subject.data An EventData@@subject.data data frame without the
# censored.at.follow.up column
# @param followup The follow period (in days)
# @return subject.data with additional censored.at.follow.up column
# Warnings will be output if dates have to be changed due to invalid data 
# for example if event occurs after followup period 
IncludeFollowUp <- function(subject.data,followup){
  
  subject.data$censored.at.follow.up <- as.numeric(subject.data$time > followup)
  subject.data$time <-ifelse(subject.data$censored.at.follow.up==1,followup,subject.data$time)
  
  
  resetToFollowUp <- function(data,idx,warning.msg){
    if(length(idx)>0){
      data$withdrawn[idx] <- 0
      data$has.event[idx] <- 0
      data$event.type[idx] <- NA
      warning(paste("Subjects",paste(data[idx,"subject"],collapse=", "),warning.msg))
    }
    data
  }
  
  subject.data <- resetToFollowUp(data=subject.data,
                   idx=which(subject.data$withdrawn==1 & subject.data$censored.at.follow.up==1),
                   warning.msg="have withdrawn dates after followup period and so have been censored at followup")
  
  resetToFollowUp(data=subject.data,
                 idx=which(subject.data$has.event==1 & subject.data$censored.at.follow.up==1),
                 warning.msg="had event after followup period and so have been censored at followup instead of having an event")

}


# Create an empty data frame for 
# event.pred.data or time.pred.data
# slots of the \code{WeibullResults} class
# @return This empty data frame
EmptyPredictionDF <- function(){
  
  data.frame(time=as.Date(character()),
                 event=numeric(),
                 CI_low=numeric(),
                 CI_high=numeric(),
                 daysatrisk=numeric())
}


# Create the text for summarizing a \code{FromDataResults object}
# @param object A \code{FromDataResults} object
# @param round.method See \code{eventPrediction:::myRound.Date}
# @param text.width numeric The width of the text to be returned. See \code{base::strwrap}
# @param show.predictions Logical if TRUE then include the time.pred.data and
# event.pred.data information in the text 
# @param show.at.risk Output the median number of at risk years
# @return A string (which can be output using cat)
getFromDataResultsSummaryText <- function(object,round.method="None",text.width=60,show.predictions=TRUE,show.at.risk=TRUE){
  
  daysinyear <- standarddaysinyear()
  
  data <- object@event.data
  indat <- data@subject.data
  indat$event.date <- ifelse(indat$has.event, LastDate(indat),NA)
  
  N.subjects <- length(indat$rand.date) #subjects in data set
  
  title <- ""
  if(N.subjects!=0){
    last.date <- if(any(!is.na(indat$event.date))) as.Date(max(indat$event.date, na.rm=TRUE),origin="1970-01-01")
                 else "NA"
    title <- paste0(N.subjects, " patients recruited where last patient in on ", max(indat$rand.date))
    
    if(class(last.date)=="Date"){
      title <- paste0(title, " and last event observed at ", last.date,
              " (",sum( indat$has.event, na.rm=TRUE), " events). ")
    }
    else{
      title <- paste0(title," and no events observed. ")
    }
  }
    
    
  if(object@Naccrual > 0){
      title <- paste0(title, 
                      " Out of ", N.subjects+object@Naccrual, " patients ",  object@Naccrual, 
                      " were simulated using ", object@accrualGenerator@text," ")
  }
  
  
  title <- paste(title, "Using ", object@simParams@type ," survival model. ",sep="")
  
  NA.note <- ""
  
  if(nrow(object@event.pred.data)!=0 && show.predictions){
    df <- object@event.pred.data 
    ans <- lapply(1:nrow(df),function(x){
      target <- myRound.Date(c(df[x,"CI_low"],df[x,"time"],df[x,"CI_high"]),round.method)
      
      retVal <- paste0("The time at which ", df[x,"event"]," events have occurred is predicted to be ",
             target[2], " [", target[1], ", ", target[3], "]" )
    
      if(show.at.risk){
        retVal <- paste(retVal," with an expected ",round(df[x,"daysatrisk"]/daysinyear,2)," years of subjects at risk at this time",sep="")
      }  
      paste(retVal,". ",sep="")
    }) 
  
    if(any(object@event.pred.data$time==as.Date(Inf,origin="1970-01-01"))){
      NA.note <- paste("An expected time of NA implies that,",
                       "in fewer than 50% of simulations the given number of events occurred.")
    }else if(any(object@event.pred.data$CI_high==as.Date(Inf,origin="1970-01-01"))){
      NA.note <- paste("A CI value of NA implies that, due to subject dropout, ",
                       "in fewer than ",100*(1-object@limit),
                        "% of the simulations the given number of events occurred.", sep="")
    }
    
    title <- paste(title,paste(ans,collapse=""),sep="")
  }
  
  if(nrow(object@time.pred.data)!=0 && show.predictions){
    df <- object@time.pred.data 
    ans <- lapply(1:nrow(df),function(x){
      retVal <- paste0("On ", df[x,"time"]," the predicted number of events is ",
             df[x,"event"], " [", df[x,"CI_low"], ", ", df[x,"CI_high"], "]" )
    
      if(show.at.risk){
        retVal <- paste(retVal," with an expected ",round(df[x,"daysatrisk"]/daysinyear,2)," years of subjects at risk",sep="")
      }  
      paste(retVal,". ",sep="")  
    }) 
    
    title <- paste(title,paste(ans,collapse=""),sep="")
  }
  
  title <- paste(title,NA.note,sep="")
  
  return(AddLineBreaks(title,text.width=text.width))
}


# Round dates
# 
# @param mydates a vector of 3 dates, the
# lower CI value, the median and the upper CI value
# @param round.method character, if "toMonths" then 
# the following procedure is applied:
# lower CI: take the month of the date 15 days earlier \cr
# median: take the month of the given date \cr
# upper CI: take the month of the date 15 days later \cr 
# otherwise the dates are unchanged
# @return The rounded dates
myRound.Date <- function(mydates,round.method){
 
  if(round.method=="toMonths"){
     c(format( mydates[1] - 15, format="%b %Y" ),
       format( mydates[2], format="%b %Y"),
       format( mydates[3] + 15, format="%b %Y" ))
  }
  else{
    mydates
  }
}


# Calculate the last date on study for subjects
# 
# Code: \code{ifelse(time==0,rand.date,time+rand.date-1)}
# 
# @param time A vector of times on study
# @param rand.date A vector of randomization dates
# @return A vector of dates
internal.Date <- function(time,rand.date){
   as.Date(ifelse(time==0,rand.date,time+rand.date-1),origin="1970-01-01")
}

# Calculate the last date on study for subjects
#
# The last date is the date of censoring or date of withdrawal
# or date of event for subjects
# @param indat  A data frame e.g. EventData@@subject.data
# @return A vector of dates
LastDate <- function(indat){
  internal.Date(indat$time,indat$rand.date)
}



# Create a data frame which contains only subjects
# who have been censored before a given date
# @param data A data frame e.g. EventData@@subject.data
# @param censor.date The given censored cutoff date
# @return The subset of \code{data} which is censored (i.e. does not
# have an event and is not withdrawn) before the given date
GetLaggedSubjects <- function(data,censor.date){
  data <- data[LastDate(data) < censor.date,]
  data[data$has.event==0 & data$withdrawn==0 & data$censored.at.follow.up==0,]
}



# Function to derive the time on study for subjects given 
# columns such as dth.date, event.date etc.
# 
# See the vignette for the logic used in calculating the time. 
# Note the changing of times so they are <= followup period is
# carried out in a subsequent function
# 
# see timeInternal.R
#
# @param data A data frame 
# @param rand.date string, the column name of randomization dates
# @param has.event string, the column name of whether a subject had an event
# (1) or not (0) 
# @param withdrawn string, the column name of whether a subject has withdrawn
# (1) or not (0)  
# @param subject, the column name of the subject ID
# @param time.list A list of column names used to calculate the time on study for example
# \code{time=list(last.date="lastDate",dth.date="dthDate", prog.date="progDate",
# withdrawn.date="withdrawnDate")}
# @return A vector containing time on study
AddTimeColumn <- function(data,rand.date,has.event,withdrawn,subject,time.list){
  
  allowed.colnames <- c("dth.date","event.date","prog.date","withdrawn.date","last.date")
  validate.time.list.arguments(data,rand.date,has.event,withdrawn,time.list,allowed.colnames)
  
  #create the date objects
  for(x in allowed.colnames){
    if(!is.null(time.list[[x]])) data[,x] <- FixDates(data[,time.list[[x]]]) 
  }
  
  #check prog is <= dth if they exist
  if("dth.date" %in% names(time.list) && "prog.date" %in% names(time.list)){
    idx <- which(!is.na(data[,"dth.date"]) & 
                 !is.na(data[,"prog.date"]) & 
                   data[,"dth.date"] < data[,"prog.date"])
    if(length(idx)>0){
      warning(paste("Subjects",paste(data[idx,subject] ,collapse=", ")  ,
                    "have progression date after death date. This is invalid and should be fixed"))
    }
  }
  
  #This vector will be populated with subjects time on study and returned 
  ans <- rep(NA,nrow(data))
  
  #first deal with subjects who are censored
  are.censored <- which(!data[,has.event] & !data[,withdrawn])
  ans <- Time.Deal.With.Censored(ans,data,rand.date,are.censored,time.list,allowed.colnames[1:4],subject)
  
  
  #next deal with subjects who have event
  subs.had.event <- which(data[,has.event]==1)
  ans <- Time.Deal.With.Had.Event(ans,data,rand.date,subs.had.event,time.list,allowed.colnames[1:3],subject,has.event)
  
  #finally subjects who have withdrawn
  has.withdrawn <- which(data[,withdrawn]==1 & data[,has.event]==0)
  ans <- Time.Deal.With.Withdrawn(ans,data,rand.date,has.withdrawn,time.list,withdrawn,subject,has.event)
  
    
  #output warning if subject has withdranw date but have not been withdrawn
  if(!is.null(time.list[["withdrawn.date"]])){
    r <- which(any(!is.na(data[,"withdrawn.date"]) & data[,withdrawn]==0))
    if(any(r)){
      warning(paste("Subjects",paste(data[r,subject],collapse=", "), "have not withdrawn yet have a withdrawn date.",
              "The withdrawn dates are ignored"))
    }
  }

  #if we have not been able to calculate a time then output
  #an error if had event or warning if censored/withdrawn
  if(any(is.na(ans))){
    r <- which(is.na(ans))
    if(any(data[r,has.event]==1)){
      s <- intersect(which(data[,has.event]==1), r)
      stop(paste("Subjects",paste(data[s,subject],collapse=", "), "have an event but no time on study can be calculated.",
                 "Is there any missing data in your data set?"))
    }
    
    warning(paste("Subjects",paste(data[r,subject],collapse=", "),
                  "are censored/withdrawn subject(s) and no time on study can be calculated.",
                  "For these subjects the time on study is set to 0. Is there any missing data",
                  "in your data set?"))
  }  
  
  return(as.numeric(ans))
      
} 


# Maximum Likelihood estimate of uniform
# accrual parameter k
# 
# @param B recruitment period
# @param s.i a vector of recruitment
# times (numeric, time since start of recruitment period)
# @return estimate of k = 1/(logB- mean(log(s.i)))
MLestimateK <- function(B,s.i){
  if(any(s.i<=0) || B <= 0 || any(s.i > B)) stop("Invalid arguments in MLestimateK")
  
  s <- mean(log(s.i))
  return(1/(log(B)-s))
}


# Calculate the average recruitment rate (subjects/day)
# @param N number of subjects recruited
# @param first.date the date of the first recruitment (numeric)
# @param last.date the date of the last recruitment (numeric)
# @return The average recruitment
average.rec <- function(N,first.date,last.date){ 
  N/(last.date-first.date+1)
}


