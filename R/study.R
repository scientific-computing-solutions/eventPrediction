#The Study class definition (used in predict from parameters)
#together with the exported methods. Also see study_constructors.R

#' @include lag.R common.R ctrlSpec.R sfn.R eventPrediction_package.R
NULL

##' Class defining the Study 
##' @slot HR Hazard ratio to be detected
##' @slot alpha Significance level [0,1] (see also two-sided indicator)
##' @slot power Power [0,1]
##' @slot two.sided If TRUE, two sided test will be used (i.e. alpha/2).
##' @slot r Control:Experimental subject balance (1:r), i.e. nE/nC=r. r=1 corresponds to equally 
##' many subjects in both arms. 2 means we have twice the number of subjects in the experimental arm.
##' Specifically \code{floor(r*N/(r+1))} subjects are 
##' allocated to the experimental arm and all other subjects are allocated to the control arm.
##' @slot N Number of subjects to be recruited (integer)
##' @slot study.duration Number of months the study will be going.
##' @slot ctrlSpec A CtrlSpec object which calculates the control group median. This object will be created automatically
##' when calling a constructor for the Study class.
##' @slot dropout A list of CtrlSpec object which calculates the median drop out rate for the control arm (index 1) and 
##' active arm (index 2). 
##' This object will be created automatically when calling a constructor for the study class
##' @slot dropout.shape The Weibull shape parameter of the dropout hazard function
##' @slot k non-uniformity of accrual (integer, 1=uniform). Non-uniform accrual is allowed for 
##' using the following distribution for the probability of a patient entering the trial at time \eqn{b} 
##' within the accrual period \eqn{[0,B]}: \eqn{F(b)=b_k/B_k}; \eqn{f(b)=k b_{k-1}/B_k} where \eqn{k} is the 
##' measure of non-uniformity (\eqn{k>0}). \eqn{k=1} indicates uniform accrual. This implies that during 
##' the first half of the accrual period, \eqn{1/2^k} of the patients will be recruited. Half of the patients 
##' will be recruited by time \eqn{B/2^{1/k}}. 
##' @slot acc.period Accrual time in months
##' @slot shape The Weibull shape parameter
##' @slot followup The time a subject is followed after randomization, if Inf then there is no fixed time period
##' @slot type Character: The study type, either "Oncology" or "CRGI"
##' @slot lag.settings The \code{LaggedEffect} object describing any lag effect for the study 
##' @export 
setClass( "Study", 
          slots= list( HR = "numeric",  # Hazard ratio
                       alpha = "numeric",
                       power = "numeric",
                       two.sided = "logical",
                       r = "numeric",  
                       N = "numeric",  # Patients to be recruited
                       study.duration = "numeric",  # Length of study (In months)
                       ctrlSpec = "CtrlSpec",  # Median of control arm
                       dropout = "list",
                       dropout.shape ="numeric",
                       k = "numeric",  
                       acc.period = "numeric",
                       shape="numeric",
                       followup="numeric",
                       type="character",
                       lag.settings = "LagEffect"),
          
          validity = function(object){
            is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol          
            ans <- ""
            if(object@shape <= 0) ans <- paste(ans,"Invalid shape.")
            if(object@acc.period <= 0) ans <- paste(ans,"Invalid acc.period")
            if(object@k <= 0) ans <- paste(ans,"Invalid k.")
            if(object@N <= 0 || !is.wholenumber(object@N) || length(object@N)>1 ) ans <- paste(ans,"Invalid N.")
            if(object@study.duration <= 0 ) ans <- paste(ans,"Invalid study.duration.")
            if(object@r < 0 ) ans <- paste(ans,"Invalid r.")
                        
            if(object@study.duration <= object@acc.period) 
              ans <- paste(ans,"acc.period must be < study.duration")
            
            if(object@r == 0){
              if(!is.na(object@HR)) ans <- paste(ans,"HR must be NA if r = 0")
              if(!is.na(object@power)) ans <- paste(ans,"power must be NA if r = 0")
              if(!is.na(object@alpha)) ans <- paste(ans,"alpha must be NA if r = 0")
            }
            else{
              if(is.na(object@HR) || object@HR >= 1 || object@HR <= 0)  ans <- paste(ans,"Invalid HR.") 
              if(object@alpha >= 1 || object@alpha <= 0 ) ans <- paste(ans,"Invalid alpha.")
              if(object@power >= 1 || object@power <= 0 ) ans <- paste(ans,"Invalid power.")
            }
            
            if(!object@type %in% c("Oncology","CRGI")) ans <- paste(ans,"Invalid type.") 
            
            if(length(object@followup) > 1 || object@followup <= 0 ) ans <- paste(ans,"Invalid followup.")
            
            if(class(object@lag.settings)!="LagEffect")ans <- paste(ans,"Invalid lag.settings")
            
            if(!isNullLag(object@lag.settings)){
              if(isSingleArm(object) && !is.na(object@lag.settings@L.HazardRatio)){
                ans <- paste(ans,"lag.settings@L.HazardRatio must be as.numeric(NA) if study has one arm")
              }
              if(!isSingleArm(object) && is.na(object@lag.settings@L.HazardRatio)){
                ans <- paste(ans,"lag.settings@L.HazardRatio cannot be NA if study has more than one arm")
              }
            }
                        
            if(!is.infinite(object@followup) && !isNullLag(object@lag.settings)){
              ans <- paste(ans, "Cannot use lagged settings with a study which has a finite follow up time")
            }
            
            if(ans=="") return(TRUE)
            ans
          }
          
)




##' @name show
##' @rdname show-methods
##' @aliases show,Study-method
##' @export
setMethod("show", signature(object="Study"), 
    function(object) {
      cat("Study definition:\n")
      cat(paste("Number of Patients (N):",object@N,"\n"))
      cat(paste("Study duration:",object@study.duration,"months\n"))
      cat(paste("Accrual period:",object@acc.period,"months\n"))
      cat(paste("Accrual uniformity (k):",object@k,"\n"))
      if(!isSingleArm(object)){
        cat(paste("Control arm survival:"),object@ctrlSpec@text,"\n")
        cat(paste("Hazard Ratio:",object@HR,"\n"))
        cat(paste("Ratio of control to experimental 1:",object@r,"\n",sep=""))
        cat(paste("alpha:",object@alpha))
        if(object@two.sided){
          cat("(two sided)\n")
        }
        else{
          cat("(one sided)\n")
        }
        cat(paste("Power:",object@power,"\n"))
      }
    else{
      cat(paste("Survival:"),object@ctrlSpec@text,"\n")
      cat("Single Arm trial\n")
    } 
      
    if(object@shape==1){
      cat("Exponential survival\n")
    }
    else{
      cat(paste("Weibull survival with shape parameter",object@shape,"\n"))    
    }
      
    if(!is.infinite(object@followup)){
      cat("Subject follow up period:",object@followup,"months\n")
    } 
      
    if(isSingleArm(object)){
      outputdropouttext("Subject",object@dropout[[1]],object@dropout.shape)
    }  
    else{
      outputdropouttext("Control Arm",object@dropout[[1]])
      outputdropouttext("Active Arm",object@dropout[[2]],object@dropout.shape)
    }
    show(object@lag.settings) 
})



##' Is the \code{Study} a single arm study
##' @rdname isSingleArm-methods
##' @name isSingleArm
##' @param study A \code{Study} object
##' @return TRUE if study is single arm, FALSE otherwise
##' @export
setGeneric("isSingleArm",
           def=function(study)
           standardGeneric("isSingleArm"))




##' @rdname isSingleArm-methods
##' @aliases isSingleArm,Study-method
##' @name isSingleArm
##' @export
setMethod("isSingleArm",representation(study="Study"),
          function(study){
            study@r==0
          })


##' @name predict
##' @rdname predict-methods
##' @aliases predict,Study-method
##' @param step.size The resolution of the grid to be used to calculate the time v 
##' expected events curves 
##' @export
setMethod( "predict", representation( object="Study" ), 
  function( object, time.pred=NULL,
            event.pred=NULL,step.size=0.5 ) {
    
    study <- object
    lagged <- study@lag.settings
    validatePredictArguments(time.pred,event.pred,step.size,study@study.duration)
        
    #times for the plotting function
    grid.times <- seq( 0, study@study.duration, step.size )
    
    #Next calculate the values of rate parameters
    lambda    <- lambda.calc( study@ctrlSpec@median, study@HR, study@shape )
    if(isNullLag(lagged)){
      lambdaot <- as.numeric(NA)
    }
    else{
      lambdaot  <- lambda.calc( lagged@ctrlSpec@median,  lagged@L.HazardRatio,study@shape )  
    }
    
    dropout.lambda <- GetDropoutLambda(study)
        
    #Create the survival functions
    sfns <- GetSurvivalFunctions(lambda,lambdaot,lagged@Lag.T,isSingleArm(study),study@shape,
                                 study@followup,study@dropout.shape,dropout.lambda)
       
    #Calculate event details for plot 
    grid <- CalculateEventsGivenTimes(grid.times,study,sfns,calc.at.risk=FALSE)  
    
    #Calculate average HR
    av.HR <-if(isNullLag(lagged) || isSingleArm(study)) study@HR 
            else calculateAverageHR(sfns,study,lagged@Lag.T,lagged@L.HazardRatio,
                                    lambdaot,lambda,study@shape)
   
    #Calculate the critical value
    critical.data <- CalculateCriticalValue(study,sfns,grid,av.HR)
    
    #Calculate events at given times
    predict.data <- if(!is.null(time.pred)) CalculateEventsGivenTimes(time.pred,study,sfns) 
                    else predict.data <- data.frame()
    
    
    #Calculate times for given number of events
    if(!is.null(event.pred)){
      predict.data <- rbind(predict.data,CalculateTimesGivenEvents(event.pred,study,sfns,grid))
    }
    
    return(AnalysisResults( 
           critical.HR = critical.data[["chr"]], 
           critical.data = critical.data[["critical.data"]], 
           critical.events.req= critical.data[["critical.events.req"]],
           av.hr=av.HR,
           grid=grid,
           predict.data=predict.data,
           study=study,
           sfns=sfns))
})

