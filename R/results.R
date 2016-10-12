#The exported methods for the results from
#predict from parameters simulations

#' @include study.R sfn.R eventPrediction_package.R
NULL

##' Class that contains results from the predict function of the \code{Study} object
##' 
##' Three of the slots of this class (grid, critical.data, predict.data) are data frames with
##' the number of events, recruitment details and cumulative time at risk at given time. See the 
##' vignette for details of these data frames and the \code{AnalysisResults} class in general.
##' 
##' @param critical.HR The critical hazard ratio at the time the critical number of events
##' is expected to occur or NA if single arm trial
##' @param critical.data A data frame with 1 row contain the time, event and recruitment details
##' for the time at which the critical number of events is expected to occur. 
##' The data frame has 0 rows if the expected time is > study duration or the trial had a single arm
##' @param critical.events.req the number of events (not rounded) required for the study's 
##' requested power and alpha or NA if single arm study
##' @param av.hr The average hazard ratio = the study hazard ratio if no lag, see the
##' vignette for how this is calculated if there is a lag. For single arm studies this is NA.
##' @param grid A data frame with rows contain the time, event and recruitment details
##' at times used to plot the event curves
##' @param predict.data A data frame with rows contain the time, event and recruitment details
##' at times / number of events requested to predicted by the user when calling the predict function.
##' @param study The \code{Study} object used to calculate the results
##' @param list The \code{Sfn} objects containing the survival functions for the results. 
##' @export
setClass( "AnalysisResults",
          slots= c( critical.HR = "numeric", critical.data="data.frame",
                    critical.events.req = "numeric", av.hr = "numeric", 
                    grid="data.frame",predict.data="data.frame",study="Study",
                    sfns="list") ) 

# Constructor for AnalysisResults class
# should not be called by users
# @param ... parameters to be passed to the new("AnalysisResults",...) function
# @return An object of type AnalysisResults
AnalysisResults <- function(...){
  new("AnalysisResults",...)
}


##' @name show
##' @rdname show-methods
##' @aliases show,AnalysisResults-method
##' @export
setMethod("show", signature(object="AnalysisResults"), 
  function(object) {
    
    show(object@study)
    if(!is.na(object@av.hr)){
      cat(paste("Average HR",round(object@av.hr,digits=2),"\n"))
    }
    
    if(nrow(object@predict.data)>0){
      cat("Predicted Values:\n")
      print(object@predict.data)
    }
    if(nrow(object@critical.data)>0){
      cat("Critical Number of Events:\n")
      print(object@critical.data)
    }
    if(!is.na(object@critical.HR)){
      cat(paste("Critical HR",round(object@critical.HR,digits=2),"\n"))
    }
   
  }
)

 
##' @name summary 
##' @param options A \code{DisplayOptions} object
##' @rdname summary-methods
##' @aliases summary,AnalysisResults-method
##' @export
setMethod('summary',signature(object="AnalysisResults") ,
  function(object, options = DisplayOptions()) {
     cat(getFromParameterText(object,options))              
  }          
)


##' @name LatexSurvivalFn
##' @rdname LatexSurvivalFn-methods
##' @aliases LatexSurvivalFn,AnalysisResults-method
##' @param decimalplaces The number of decimal places to output for the 
##' rate and shape parameters
##' @export
setMethod("LatexSurvivalFn",
    signature("AnalysisResults"),
    function(results,decimalplaces=3){
            
      shape <- c("a","\\alpha") 
       
      if(!isSingleArm(results@study)){
        ans <- "Control Arm:\n"
        lambda <- c("\\lambda_{c1}","\\lambda_{c2}")
      }
      else{
        ans <- ''
        lambda <- c("\\lambda_1","\\lambda_2")
      }
      lambda <- c(lambda,"\\lambda_d") 
      
                  
      ans <- paste(ans,LatexSurvivalFn(results@sfns[[1]],decimalplaces=decimalplaces,lambda=lambda,shape=shape))
             
      if(!isSingleArm(results@study)){
        lambda <- c("\\lambda_{e1}","\\lambda_{e2}","\\lambda_d")
        ans <- paste(ans,"Experimental Arm:\n")
        ans <- paste(ans,LatexSurvivalFn(results@sfns[[2]],decimalplaces=decimalplaces,lambda=lambda,shape=shape))
      }
      
      if(results@sfns[[1]]@dropout.lambda!=0){
        ans <- paste(ans,"Dropout rate given by \\( \\lambda_d \\)")
        if(results@sfns[[1]]@dropout.shape!=1){
          ans <- paste(ans,"and shape given by \\( \\alpha \\)")
        }
        ans <- paste(ans,".",sep="")
      }      
      ans
    }
)


