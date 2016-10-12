#Some miscellaneous functions used by both
#predict form parameters and predict from data

##' @importFrom stats uniroot
NULL

##' Return the number of days in the year 
##' 
##' This allows the user control over
##' whether the package uses 365.25, 365 (or any other number) of days
##' in a year. 
##' 
##' The number of days in a month is then set as daysinyear()/12 
##' 
##' @return The number of days in the year
##' @export 
standarddaysinyear <- function() {
  getOption('eventPrediction.daysinyear',
            365.25)
}


# Round a number and force trailing zeros to be output
# @param number The number to be output
# @param dp The number of digits to display after the decimal point
# @return A string (e.g. "43.400")
roundForceOutputZeros <- function(number,dp){
  if(dp < 1) {
    stop("Invalid argument dp must be positive")
  }
  roundnumber <- as.character(round(number,digits=dp))
  outputdp <- nchar(strsplit(as.character(roundnumber),"\\.")[[1]][2]) 
  zeros <- rep("0",dp-outputdp)
  return(paste(roundnumber,paste(zeros,collapse=""),sep=""))
  
}

# Calculate the number of subjects on each arm
# 
# \code{floor((r/(r+1))*N)} are allocated to the experimental arm
# the rest are allocated ot the control arm
# 
# @param singleArm logical TRUE if study is a single arm
# @param r Allocation ratio, see Study object
# @param N Total number of subjects on the trial
# @return A vector (N_control,N_experimental)
getNs <- function(singleArm,r,N){
  if (singleArm){
    return(c(N,0))
  }
  
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  
  oldres <- (r/(r+1))*N
  res <- if(!is.wholenumber(oldres)) floor(oldres) else oldres
  res <- c(N-res,res)
  
  if(any(res==0)){
    stop("Too few subjects recruited for given randomization balance
         All subjects would be recruited onto the same arm.")
  }
  
  return(res)
}

# Add line breaks to a string
# @param title text to add line breaks to
# @param text.width Target number of characters per row
# @return text with line breaks
AddLineBreaks <- function( title, text.width ) {
  wrapper <- function(x, ... ) paste(strwrap(x, ... ) ) 
  wtext <- wrapper(title, width = text.width )
  return(paste(wtext,collapse="\n"))
}

##' Function which attempts to read in a csv file
##' picking a delimiter from a given set of options
##' 
##' The function works by counting the number of occurences 
##' of each delimiter on each line and for exactly one 
##' of the delimiter options all lines contain the same number
##' of occurences (>0) then this delimiter is used
##' 
##' @param path The path to the csv file to be read
##' @param delim_options A vector of characters from which the
##' delimiter should be chosen
##' @return If successful a data frame containing the csv file
##' if unsuccessful an error is thrown 
##' @export
csvSniffer <- function(path,delim_options=c(',',';','\t')){
  #currently reading file twice ought to be improved
  vec <- readLines(con=path)
  #remove empty lines
  vec <- vec[sapply(vec, nchar) > 0] 
  ans <- sapply(delim_options,function(x){
    a <- unlist(lapply(vec,function(y)sum(unlist(strsplit(y,split=""))==x)))
    if(all(a==a[1]) && a[1] != 0) TRUE else FALSE  
  })
  if(sum(ans) != 1){
    stop("Cannot determine delimiter")
  }
  read.csv(path,sep=names(ans[ans==TRUE])) 
}


##' Given trial assumptions describing an exponential model
##' Use the method of moments to fit a single Weibull distribution
##' 
##' See, for example, Lei, Y. Evaluation of three methods for estimating the Weibull distribution 
##' parameters of Chinese pine (Pinus tabulaeformis). Journal of Forest Science 54.12 (2008): 566-571.
##' 
##' @param HR Hazard ratio
##' @param r Randomization balance
##' @param M control arm median 
##' @export
FitMixtureModel <- function(HR,r,M){
  
  lapply(list(HR=HR,r=r,M=M),function(x){
    if(!is.numeric(x) || x <= 0 || length(x)!=1){
      stop("All arguments to FitMixtureModel must be numeric, positive and of length 1")
    }
  })
  
  ml2 <- M/log(2)
  mu <- (ml2*(1+r/HR))/(r+1) # = E(control)/(r+1) + rE(active)/r+1
  sigma <- (ml2/(1+r)) * sqrt(1+2*r-2*r/HR + (r*(r+2))/(HR*HR))
  
  return(RootFindRateShape(mu,sigma))
}


# Fit shape and rate given mean and sd of Weibull
# distribution
# 
# @param mu mean of Weibull
# @param sigma sd of distribution  
RootFindRateShape <- function(mu,sigma){
  f <- function(x){
    sigma/mu - sqrt(gamma(1+2/x) - gamma(1+1/x)^2)/gamma(1+1/x)  
  }

  shape <- tryCatch({ 
    uniroot(f,c(0.1,10))$root
  },error=function(cond){
    warning(paste("Error fitting shape:",cond,"Returning NA"))
    return(list(rate=NA,shape=NA))  
  }
  )

  rate <- gamma(1+1/shape)/mu
  return(list(rate=rate,shape=shape))
}
