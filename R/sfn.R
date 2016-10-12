#Code for the predict from parameters Survival functions/
#functions to be used in the integral when calculating the
#expected event times and times at risk


##' A Class containing the Survival Function (for a single arm) 
##' used in the integral to calculate event times
##' in predict from parameters. It should not be created by the user
##' but is created for the user (see sfns slot of \code{AnalysisResult})
##' 
##' @slot sfn A function sfn(x) is the function to be included in the events.integrate 
##' procedure when using predict from parameters. 
##' Specifically sfn(x) =  1 - P(had event [i.e. not dropout] by time x) and when no drop outs 
##' this is exactly the survival function.
##' If using dropouts this is not quite the survival function
##' However, the output of LatexSurvivalFn(object) is the survival function. 
##' @slot SurvivalFunction The actual survival function (if drop outs/finite followup are used then this will
##' not equal sfn). If finite follow up is used then S(x) = 0 for x > followup
##' @slot pdf The pdf function associated with the survival function. If finite follow up is used
##' then pdf(x) should  = 0 for x > followup 
##' @slot nullf Logical, TRUE if the object represents NULL (i.e. a survival function for 
##' a second arm in a single arm study)
##' @slot lambda The rate parameters for the arm of trial.
##' In a trial with lag, this is the rate parameters for time > T
##' @slot lambdaot If a lag was used then the rate parameters 
##' for time < T otherwise NA
##' @slot shape The Weibull shape parameter 
##' @slot LagT The lagtime for the survival function (0 for no lag) 
##' @slot followup The follow up time for each subject 
##' (Inf for studies with no fixed followup)
##' @slot dropout.shape The Weibull shape parameter for the drop out hazard function 
##' @slot dropout.lambda The rate parameter for the drop out hazard function = 0 if no dropout 
##' 
##' @export  
setClass("Sfn", 
          slots=list(sfn="function",
                     SurvivalFunction="function",
                     pdf="function",
                     nullf="logical",
                     lambda="numeric",
                     lambdaot="numeric",
                     shape="numeric",
                     LagT="numeric",
                     followup="numeric",
                     dropout.shape="numeric",
                     dropout.lambda="numeric"
                )  
)

# Constructor for \code{Sfn} object
# @param lambda The rate parameters for the arm of the trial.
# In a trial with lag, this is the rate parameters for time > T
# @param lambdaot If a lag was used then the rate parameters 
# for time < T otherwise NA
# @param lag.T The lagtime for the survival function (0 for no lag) 
# @param shape The Weibull shape parameter 
# @param followup The follow up time for each subject (Inf for studies with no fixed followup)
# @param dropout.shape The Weibull shape parameter for the drop out hazard function 
# @param dropout.lambda The rate parameter for the drop out hazard function - 0
# if no drop out
Sfn <- function(lambda,lambdaot,lag.T,shape,followup,dropout.shape=1,dropout.lambda=0){
  
  if(lag.T==0){
    lambdaot <- as.numeric(NA)
  }
  
  #survival function without dropouts or fixed follow up...
  f <- if(lag.T==0) 
         function(x){
           return(exp(-(lambda*x)^shape))
         }
       else 
         function(x){
           ifelse(x<lag.T, exp(-(lambdaot*x)^shape),exp(-(lambda*x)^shape+(lambda^shape-lambdaot^shape)*lag.T^shape))
         }
  
  #... and its corresponding hazard function 
  h <- if(lag.T==0) 
         function(x){
           return(shape*lambda^shape*x^(shape-1))
         }
       else 
         function(x){
           ifelse(x<lag.T,shape*lambdaot^shape*x^(shape-1),shape*lambda^shape*x^(shape-1))
         }
  
  ################################################################
  
  #next we calulate the function giving 1- P(having event) which is placed in the sfn slot 
  #of the object when including dropouts this function is the integrated or cumulative hazard for hazard
  #event. 
  
  #The integrand of the cumulative hazard function
  f2 <- function(x){h(x)*f(x)*exp(-(x*dropout.lambda)^dropout.shape)}
  
  #we precompute the value of the integral when there is dropout and lag
  lag.T.int <- if(lag.T != 0 && dropout.lambda != 0) integrate(f=f2,lower=0,upper=lag.T)$value else 0
  
  #we precompute the value of 1-P(having event by time=follow up) to save calculating the integral
  #multiple times
  follow.up.int <- if(dropout.lambda!=0 && !is.infinite(followup)) 
                      integrate(f=f2,lower=0,upper=followup)$value else 0 
  
  
  #include drop out and follow up if required for slot sfn (so no longer the survival function)
  dropf <- function(x){
    #First deal with case of no dropout
    if(dropout.lambda==0){
      return(ifelse(x<followup,f(x),f(followup)))
    }
        
    #next no lag but followup 
    if(lag.T==0 && !is.infinite(followup)){
      return(sapply(x, function(t){
        ifelse(t < followup,
          1-integrate(f=f2,lower=0,upper=t)$value,
          1-follow.up.int) #1-P(have event by time t > followup) = 1-P(have event by time followup)
      }))
    }
    
    #Next no lag or followup < lag (so no discontinuity)
    if(lag.T==0 || followup <= lag.T){    
      return(sapply(x,function(t){1-integrate(f=f2,lower=0,upper=min(t,followup))$value}))
    } 
    
    #finally case with lag and follow up > lag   
    sapply(x, function(t){
      ifelse(t < lag.T,
          1-integrate(f=f2,lower=0,upper=min(t,followup))$value,  
          1-integrate(f=f2,lower=lag.T,upper=min(t,followup))$value-lag.T.int #split integral into two parts   
      )
    })
      
  }
  #the function to be included in the events integral
  sfn <- dropf

  ########################################
  
  #Now we deal with the Survival function
  #Add drop outs
  S <- function(x){
    if(dropout.lambda==0){
      return(f(x))
    }
    f(x)*exp(-(x*dropout.lambda)^dropout.shape)
  }
  
  #and fixed follow ups
  #In '<=' the equal sign here is crucial as we need 
  #to use S(followup) when calculating the atrisk information
  SurvivalFunction <- function(x){ifelse(x<=followup,S(x),0)} 
  
  #######################################
  
  ##Next work on pdf
    
  dropouth <- function(x){
    if(dropout.lambda==0){
      return(0)
    }
    return((dropout.lambda*dropout.shape)*(dropout.lambda*x)^(dropout.shape-1))
  }
  
  #pdf is hazard function * survival function
  pdfwithdropout <- if(dropout.lambda==0) function(x){S(x)*h(x)} else function(x){S(x)*(h(x)+dropouth(x))}
      
  ##and fixed follow up
  pdf <- if(is.infinite(followup)) pdfwithdropout else function(x){ifelse(x<=followup,pdfwithdropout(x),0)}
  
  new("Sfn",sfn=sfn,nullf=FALSE,
      lambda=lambda,lambdaot=lambdaot,LagT=lag.T,shape=shape,followup=followup,
      dropout.lambda=dropout.lambda,dropout.shape=dropout.shape,SurvivalFunction=SurvivalFunction,pdf=pdf)
  
}

# Create a Null version of the \code{Sfn} object 
# For use as the survival function for the experimental arm
# of a single arm trial
# @return A \code{Sfn} object
NullSfn <- function(){
  f <- function(x){0}
  new("Sfn",sfn=f,nullf=TRUE,lambda=0,lambdaot=0,LagT=0,shape=0,followup=0,
      dropout.shape=1,dropout.lambda=0,SurvivalFunction=f,pdf=f)
}

##' Method to output a Latex String of the object
##' 
##' @param results Object to output 
##' @param \ldots Additional parameters to be passed into the function
##' @param lambda The symbol to be used for the arm's rate 
##' parameter, if lagged study then this is a vector of before and after lag
##' symbols. The latex backslash character needs to be escaped  
##' @param shape The symbol to be used for Weibull shape parameter
##' @return A latex string of the object (for \code{Sfn} this is the Survival function)
##' @rdname LatexSurvivalFn-methods
##' @name LatexSurvivalFn
##' @export
setGeneric( "LatexSurvivalFn", 
            def = function( results, ... )
              standardGeneric( "LatexSurvivalFn" ))


##' @name LatexSurvivalFn
##' @rdname LatexSurvivalFn-methods
##' @aliases LatexSurvivalFn,Sfn-method
##' @export
setMethod("LatexSurvivalFn",
  signature("Sfn"),
  function(results,decimalplaces,lambda,shape){
    
    #First set the function which will produce the output 
    if(results@LagT!= 0 && results@shape ==1 ){
      LatexOutput <- function(lambda,shape,followup,dropouttext){
        return(paste("$$ S(t) = \\begin{cases} \\exp(-",lambda[1],"t",dropouttext,") & \\quad \\text{if } t \\le T \\\\
                     \\exp(-",lambda[2],"t-(",lambda[1],"-",lambda[2],")T",dropouttext,")  & \\quad \\text{if }  t > T\\ \\end{cases} $$"))
      }
    }else if(results@LagT!= 0 && results@shape !=1){
      LatexOutput <- function(lambda,shape,followup,dropouttext){
        return(paste("$$ S(t) = \\begin{cases} \\exp(-(",lambda[1],"t)^",shape,dropouttext,") & \\quad \\text{if } t \\le T \\\\
                     \\exp(-(",lambda[2],"t)^",shape,"-(",lambda[1],"^",shape,
                     "-",lambda[2],"^",shape,")T^",shape,dropouttext,")  & \\quad \\text{if }  t > T\\ \\end{cases} $$"))
      }
    }else if(results@LagT == 0 && results@shape ==1){  
      LatexOutput <- function(lambda,shape,followup,dropouttext){
          fuptxt <- if(is.infinite(followup)) "" else paste("\\quad (\\text{if } t <",round(followup,decimalplaces),")") 
          return(paste("$$S(t) = \\exp(-",lambda[1],"t",dropouttext,")",fuptxt,"$$"))
      }
    }else{    
      LatexOutput <- function(lambda,shape,followup,dropouttext){
        fuptxt <- if(is.infinite(followup)) "" else paste("\\quad (\\text{if } t <",round(followup,decimalplaces),")") 
        return(paste("$$S(t) = \\exp(-(",lambda[1],"t)^",shape,dropouttext,")",fuptxt,"$$"))
      }
    }
    
    #Then produce the output
    ans <- ""
    extra <- ""
    
    dropouttext <- ""
    if(results@dropout.lambda!=0){
      
      if(results@dropout.shape==1){
        dropouttext <- paste("-",lambda[3],"t")
      }
      else{
        dropouttext <- paste("-(",lambda[3],"t)^",shape[2])
      }
    }
    
    
    if(results@LagT!=0){
      extra <- paste(",\\:",lambda[2],"=",round(results@lambda,decimalplaces))
      lambda_val <- round(results@lambdaot,decimalplaces)
    }
    else{
      lambda_val <- round(results@lambda,decimalplaces)
    }    
    
    ans <- paste(ans,LatexOutput(lambda,shape=shape[1],followup=results@followup,dropouttext),"\n")
    ans <- paste(ans,"$$",lambda[1],"=",lambda_val,extra)
    
    if(results@shape !=1) {
      ans <- paste(ans,",\\:",shape[1],"=",round(results@shape,decimalplaces))
    }
    
    if(results@dropout.lambda!=0){
      ans <- paste(ans,",\\:",lambda[3],"=",round(results@dropout.lambda,decimalplaces))
      
      if(results@dropout.shape!=1){
        ans <- paste(ans,",\\:",shape[2],"=",round(results@dropout.shape,decimalplaces))
      }
    }
    
    ans <- paste(ans,"$$\n") 
    return(ans)
  }
)

# Calculate the expected time at risk
# 
# @param SurvFn sfn object. If nullf slot of sfn object is true 
# then 0 time at risk occurs occur (this is used
# when calculating single arm trials) 
# @param B Accrual time.
# @param k Non-uniformity of accrual (numeric, 1=uniform).
# @param t A vector of prediction times for which the expected time  
# at risk is required
# @return A vector of time at risk at the given times
atrisk.integ <-  function( SurvFn, B, k, t ) {
  if(SurvFn@nullf){
    return (rep(0,length(t)))
  }
  
  #see the predict from parameters vignette for the three formula calculated here
  #and their derivations
  
  #####################
  
  #First calculate the amount of time of subjects who are still on the trial at time t
  #= integral from 0 to min(t,B) of P(still on trial at t | rec at s)P(rec at s)*time_at_risk ds
  #and time_at_risk=t-s
  myf1 <- function(s,k,t){(k*s^(k-1)/B^k)*SurvFn@SurvivalFunction(t-s)*(t-s)}
  
  i1 <- unlist(lapply(t,
                      function(x){
                        if(x==0) 0 else integrate(f=myf1,lower=0,upper=min(x,B),k=k,t=x)$value
                      })) 
  
  #Next calculate the amount of time of subjects who had an event or dropped out before time t
  #This does not include subjects who have dropped out as they were censored at the end of the follow up period
  
  #This function is given by 
  #integral from 0 to t [ W ] dt' where W = time at risk for subject who had event at time t'
  #specifically
  # W = integral from 0 to t' P(had event at time t' | rec at s)P(rec at s)(t'-s)  ds
  #notw the "at" in P(had event at time t') implies we need the pdf rather than the survival function
  
  internalf <- function(s,t,k,B){sapply(s,function(x,t,k,B){(k*x^(k-1)/B^k)*SurvFn@pdf(t-x)*(t-x)},t=t,k=k,B=B)}
  myf2 <- function(t,k,B){sapply(t,function(x,k,B){
    upper <- min(x,B)
    lower <- max(x-SurvFn@followup,0)
    lagpoint <- max(x- SurvFn@LagT,0)
    if(lower > upper){
      return(0)
    }
    
    if(lagpoint >= upper || lagpoint <= lower){
      return( integrate(internalf,lower=lower,upper=upper,t=x,k=k,B=B)$value)
    }
    
    integrate(internalf,lower=lower,upper=lagpoint,t=x,k=k,B=B)$value+
      integrate(internalf,lower=lagpoint,upper=upper,t=x,k=k,B=B)$value},k=k,B=B)
    
    
  }
  
  i2 <- unlist(lapply(t,function(x){
    if(x==0) 0 else integrate(f=myf2,lower=0,upper=x,k=k,B=B)$value  
  }))
  
  #Finally calculate the time on study for subjects who dropped out at the follow up period time before time t
  #The three terms multiplied are: 
  #P(surviving until F)
  #Time spent at risk (i.e. F)
  #P(recruited before time t-F), 
  i3 <- unlist(lapply(t,function(x){
    if(x < SurvFn@followup) 0 else SurvFn@SurvivalFunction(SurvFn@followup)*SurvFn@followup*(min(B,x-SurvFn@followup)^k)/B^k
  }))
  
  #And sum the three values together
  i1+i2+i3
}

# Calculate expected events using adaptive quadrature
# 
# This function uses R's integrate function to calculate
# the integral s^{k-1}*SurvFn(t-s) ds for given
# k, t and SurvFn. The limits of the integral are 0 to 
# min(t,B)
# 
# @param SurvFn sfn object. If nullf slot of sfn object is true 
# then 0 events occur (this is used
# when calculating single arm trials) 
# @param B Accrual time.
# @param k Non-uniformity of accrual (numeric, 1=uniform).
# @param t A vector of prediction times for which the number 
# of events is required
# @return A vector of number of events at the given times
events.integ <- function( SurvFn, B, k, t ) {
  
  if(SurvFn@nullf){
    return (rep(0,length(t)))
  }
  
  #the function to be integrated (s is the dummy variable)
  #see vignette
  myf <- function(s,k,t){s^{k-1}*SurvFn@sfn(t-s)}
  
  
  integrated_val <- unlist(lapply(t,
           function(x){
             if(x==0){
               return(0) #integrate unhappy if both limits are 0 and k < 1
             }
             upperlim <- min(x,B)
             
             #If integrand is differentiable over limits then integrate in one go
             if(SurvFn@LagT == 0 || upperlim <= SurvFn@LagT  ){
               return(integrate(f=myf,lower=0,upper=upperlim,k=k,t=x)$value)
             }
                                    
             #when there is a lag the integrand is non-differentiable
             #so splitting the limits into two differentiable parts 
             #vastly improves performance
             split.point <- upperlim - SurvFn@LagT
             return(integrate(f=myf,lower=0,upper=split.point,k=k,t=x)$value+
                    integrate(f=myf,lower=split.point,upper=upperlim,k=k,t=x)$value)
                                    
             }       
  ))
  #See vignette for how integrated_val is used
  (pmin(t,B)/B)^k - k*integrated_val/B^k
}


 