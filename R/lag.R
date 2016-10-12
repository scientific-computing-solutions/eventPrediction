#In predict from parameters it is possible to include a 
#lag in the study definition when calculating event times
#the code associated with this lag is contained here.


#' @include ctrlSpec.R
NULL

##' Parameter settings for when including a lagged effect. 
##' 
##' Note the lambda and HR in the \code{Study} class will be used
##' for time > T
##' @slot Lag.T Lagtime (T)
##' @slot ctrlSpec Control median specification for time period [0,T]
##' @slot L.HazardRatio Hazard for time period [0,T]
##' @seealso \code{\link{show,LagEffect-method}}
##' @export
setClass( "LagEffect", 
          slots=c( 
            Lag.T = "numeric",
            ctrlSpec = "CtrlSpec",
            L.HazardRatio = "numeric"
                     
          ), prototype = prototype( 
            Lag.T = 0,
            ctrlSpec = CtrlSpecfromMedian(as.numeric(NA)),
            L.HazardRatio = as.numeric(NA)
          ),
          validity = function(object){
            ans <- ""
            if(object@Lag.T < 0) ans <- paste(ans,"Invalid Lag.T")
            if(!is.na(object@L.HazardRatio) && (object@L.HazardRatio < 0 || object@L.HazardRatio > 1))
              ans <- paste(ans,"Hazard Ratio must be in [0,1]")
            
            if(object@Lag.T!=0){
              if(is.na(object@ctrlSpec@median)){
                ans <- paste(ans,"Invalid LagEffect") 
              } 
            }
            else{
              if(!is.na(object@ctrlSpec@median) || !is.na(object@L.HazardRatio)){
                ans <- paste(ans,"Invalid nullLagEffect") 
              } 
            }
            
            if(ans=="") return(TRUE)
            ans
            
          }
)



##' @name show
##' @rdname show-methods
##' @aliases show,LagEffect-method
##' @export
setMethod("show", signature(object="LagEffect"), 
  function(object) {
    if(isNullLag(object)){
      cat("No Lag\n")
    }
    else{
      cat(paste(object@Lag.T,"months of lag during which\n"))
      cat(paste("control group survival",object@ctrlSpec@text,"months\n"))
      if(!is.na(object@L.HazardRatio)){
        cat(paste("and the hazard ratio is",object@L.HazardRatio,"\n"))
      }
          
    }
  }
)
          

##' LagEffect constructor
##'
##' Note the lambda and HR in the study class will be used
##' for time > T
##' @param Lag.T Lagtime (T)
##' @param L.Ctr.median Control median for time period [0,T]
##' @param L.HazardRatio Hazard for time period [0,T]
##' @return A LagEffect object
##' @export
LagEffect <- function(Lag.T,L.Ctr.median=as.numeric(NA),L.HazardRatio=as.numeric(NA)){
  ctrlSpec <- CtrlSpecfromMedian(L.Ctr.median)
  new("LagEffect",Lag.T=Lag.T,ctrlSpec=ctrlSpec,L.HazardRatio=L.HazardRatio)
}

##' Create a LagEffect object with no lag
##' @return A LagEffect object for which \code{isNullLag()} is TRUE
##' @export
NullLag <- function(){
  NAnumeric <- as.numeric(NA)
  LagEffect(Lag.T=0,L.Ctr.median=NAnumeric,L.HazardRatio=NAnumeric)
}

##' Function to check whether Lag is a null lag
##' 
##' This checks whether the Lag.T slot is 0 
##'    
##' @name isNullLag
##' @docType methods
##' @rdname isNullLag-methods
##' @param Lag A LagEffect object
##' @return TRUE if null lag, FALSE otherwise
##' @export
setGeneric( "isNullLag",
            def=function( Lag )
              standardGeneric( "isNullLag" ) )


##' @rdname isNullLag-methods
##' @aliases isNullLag,LagEffect-method
##' @name isNullLag
##' @export
setMethod( "isNullLag",signature=(Lag="LagEffect"), 
  function( Lag ) {
    Lag@Lag.T==0
})

