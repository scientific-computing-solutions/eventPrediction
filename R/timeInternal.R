#Functions called by AddTimeColumn - which calculates the 
#time on study for EvewntData object creation are found here

# Validates the function arguments to AddTimeColumn
# @inheritParams AddTimeColumn
# @param allowed.colnames The names allowed in the time.list argument
validate.time.list.arguments <- function(data,rand.date,has.event,withdrawn,time.list,allowed.colnames){
  if(length(time.list)==0){
    stop("Empty time list argument")
  }
  
  if (!all(names(time.list) %in% allowed.colnames)){
    stop(paste("Invalid options to time argument they must be",paste(allowed.colnames,collapse=" ")))
  }
  
  if(!"last.date" %in% names(time.list) ){
    stop("last.date option must be included")
  }
  
  for(x in time.list ){
    if(!x %in% colnames(data)){
      stop(paste("Column name",x,"not found in data frame"))
    }
  }
  
  if(any(!data[,has.event] %in% c(0,1))){
    stop("All subjects must have 0 or 1 in hasEvent column")
  }
  if(any(!data[,withdrawn] %in% c(0,1))){
    stop("All subjects must have 0 or 1 in withdrawn column")
  }
}


# Derive the time of the subjects who are censored
# @inheritParams AddTimeColumn
# @param are.censored A vector of indices of subjects who are censored
# @param warning.colnames The names of columns in the time.list argument which
# will causes warnings if they contain data foe subjects who are censored
# @param ans A vector of calculated times
# @return ans with times added for subjects who are censored
Time.Deal.With.Censored <- function(ans,data,rand.date,are.censored,time.list,warning.colnames,subject){
  
  if(length(are.censored) == 0){
    return(ans)  
  }  
  
  ans[are.censored] <- data[are.censored,"last.date"] - data[are.censored,rand.date] + 1
  
  for(x in warning.colnames){
    if(!is.null(time.list[[x]])){
      r <- intersect(which(!is.na(data[,x])),are.censored)
      if(any(r)){
        warning(paste("Subjects",paste(data[r,subject],collapse=", "),
                      "do not have an event and are not withdrawn and have data in column",x,"which is ignored"))
      }
    }
  }
  ans
}


# Derive the time of the subjects who have withdrawn
# @inheritParams AddTimeColumn
# @param has.withdrawn A vector of indices of subjects who have withdrawn
# @param ans A vector of calculated times
# @return ans with times added for subjects who are censored
Time.Deal.With.Withdrawn <- function(ans,data,rand.date,has.withdrawn,time.list,withdrawn,subject,has.event){
  if(length(has.withdrawn)==0){
    return(ans) 
  }  
  

  if(is.null(time.list[["withdrawn.date"]])){
    warning(paste("Some subjects have withdrawn and there is no withdrawn date column, using last date instead"))
    ans[has.withdrawn] <- data[has.withdrawn,"last.date"] - data[has.withdrawn,rand.date] + 1
    return(ans)
  }
  
  ans[has.withdrawn] <- data[has.withdrawn,"withdrawn.date"] - data[has.withdrawn,rand.date] + 1
    
  r <- is.na(data[,"withdrawn.date"])& data[,withdrawn]==1 & data[,has.event]==0
  if(any(r)){
    warning(paste("Subjects",paste(data[r,subject],collapse=", "), "who have withdrawn do not have a withdrawn date,",
                  "the subjects last.date is used"))
      
    ans[r] <- data[r,"last.date"] - data[r,rand.date] + 1
  }
    
    
  #warn of any inconsistencies between last.date and actual.date
  inconsistent <- which(!data[,has.event] & data[,withdrawn] & !is.na(data[,"last.date"]) & 
                        ans > data[,"last.date"] - data[,rand.date] +1 )
    
  if(length(inconsistent)>0){
    warning(paste("Subjects",paste(data[inconsistent,subject],collapse=", "),
                  "have lastdate earlier than withdrawnDate. ",
                  "The lastdate column is ignored"))
  }
    
  ans
}  
  





# Derive the time of the subjects who have an event
# @inheritParams AddTimeColumn
# @param subs.had.event A vector of indices of subjects who had an event
# @param event.colnames The names of columns in the time.list argument which
# contain event details
# @param ans A vector of calculated times
# @return ans with times added for subjects who had event
Time.Deal.With.Had.Event <- function(ans,data,rand.date,subs.had.event,time.list,event.colnames,subject,has.event){
  if(length(subs.had.event)==0){
    return(ans)  
  }  
  
  for(x in event.colnames){
    if(!is.null(time.list[[x]])){
      ans <- ifelse(data[,has.event],
                    pmin(ans,data[,x] - data[,rand.date] + 1,na.rm=TRUE),
                    ans)
    }
  }
  
  #who has to use last.date
  use.last.date <- which(data[,has.event] & is.na(ans) & !is.na(data[,"last.date"]))
  
  if(length(use.last.date)>0){
    warning(paste("Subjects",paste(data[use.last.date,subject],collapse=", "),
                  "have an event but do not have a date of their event.",
                  "The lastdate column is used"))
    ans[use.last.date] <- data[use.last.date,"last.date"] - data[use.last.date,rand.date] + 1
    
  }
  
  #warn of any inconsistencies between last.date and actual.date
  inconsistent <- which(data[,has.event] & !is.na(data[,"last.date"]) & 
                          ans > data[,"last.date"] - data[,rand.date] +1 )
  
  if(length(inconsistent)>0){
    warning(paste("Subjects",paste(data[inconsistent,subject],collapse=", "),
                  "have lastdate earlier than the date of their event.",
                  "The lastdate column is ignored"))
  }
  
  ans
}