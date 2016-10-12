#This file contains the definition of the class required for simulating
#subject event times in the from Data part of the package -i.e. the rate and
#shape used in the condtional Weibull distribution

setOldClass("survreg")

##' An S4 class containing the function required
##' to simulate conditional survival times
##' @slot type A character string describing the distribution used
##' for example "Weibull"
##' @slot generateParameterFunction A function with a single parameter Nsim which returns
##' a matrix with Nsim rows and columns Id(=1:Nsim) and the specific parameters needed by
##' conditionalFunction below. In the Weibull case these are the specific rate and shape 
##' parameters used for each simulation
##' @slot conditionalFunction A function with the following arguments t.conditional, params, HR.
##' t.conditional is a vector of current survival times, params is a row of the matrix returned
##' by generateParameterFunction and HR is a vector of 1s for subjects in control arm and the hazard ratio 
##' for subjects in the active arm (if HR argument to simulate is NULL then this is a vector of 1s). The function
##' then returns a vector of event times. 
##' @slot parameters A list of parameters used for the simulation. For the Weibull this is
##' rate, shape and sigma (the uncertainty matrix of the parameters - specifically the covariance matrix from survreg)
##' @aliases FromDataSimParam.class
##' @export
setClass("FromDataSimParam", 
         slots=list(type="character",
                    conditionalFunction="function",
                    generateParameterFunction="function",
                    parameters="list")
)


# Sample from a conditional Weibull distribution.
#
# This function is used as the conditionalFunction for
# a Weibull FromDataSimParam simulation and also used
# to simulate subject dropouts
#
# cdf: \code{F(t) = 1 - exp(-(t*rate)^shape)}
# subject to \code{t > t.conditional}
# Therefore all results will be \code{>t.conditional}
# @param t.conditional A vector of survival times subjects have already survived until
# @param params A named vector of parameters including shape and rate
# @param HR A vector of Hazard ratios for each subject (if =1 then subject is on control/only) arm
# @return A vector of survival times 
rcweibull <- function(t.conditional, params, HR) {
  params <- as.list(params)
  params$rate <- params$rate*(HR)^{1/params$shape}
  t.conditional <- t.conditional*params$rate
  ((t.conditional^params$shape + rexp(length(t.conditional)))^(1/params$shape))/params$rate
}

# Sample from a conditional loglogistic distribution.
#
# This function is used as the conditionalFunction for
# a loglogistic FromDataSimParam simulation and also used
# to simulate subject dropouts
#
#
# @param t.conditional A vector of survival times subjects have already survived until
# @param params A named vector of parameters including shape and rate
# @param HR A vector of Hazard ratios for each subject must be a vector of 1's for 
# the loglogistic, see rcweibull for more details
# @return A vector of survival times 
rcloglogistic <- function(t.conditional,params,HR){
  if(any(HR!=1)){
    stop("Cannot use HR argument with loglogistic model")
  }
  params <- as.list(params)
  
  retVal <- 1+(t.conditional*params$rate)^params$shape
  retVal <- retVal/runif(length(t.conditional)) - 1
  retVal^(1/params$shape)/params$rate

}


##' Method to create FromDataSimParam objects
##' @param object The object to derive the fromDataParam
##' @param type The type argument to be used in the FromDataSimParam type slot
##' @param ... Additional options to be passed into the function
##' @rdname FromDataParam-methods
##' @name FromDataParam
##' @export
setGeneric("FromDataParam",function(object,type,...) standardGeneric("FromDataParam"))

##' Create FromDataSimParam object from survreg model
##' @rdname FromDataParam-methods
##' @name FromDataParam
##' @aliases FromDataParam,survreg,character-method
##' @export
setMethod("FromDataParam",signature=c(object="survreg",type="character"),
  function(object,type){
            
    if(!object$dist %in% c("weibull","loglogistic")){
      stop("Invalid model for FromDataSimParam function")
    }
    
    if(object$dist != type){
      stop("Argument mismatch in FromDataParam")
    }
            
    rate <-  1/exp(object$coefficient)
    shape <- 1/object$scale
    sigma <- object$var 
            
    FromDataParam(type=type,rate=rate,shape=shape,sigma=sigma)
            
  }
)

##' @param rate The (mean) Weibull/loglogistic rate parameter for the simulations
##' @param shape The (mean) Weibull/loglogistic shape parameter for the simulations
##' @param sigma Advanced: The covariance (uncertainty) matrix used to sample single
##' simulation rates and shapes. The covariance matrix describes the uncertainty of
##' {-log(rate),-log(shape)} - i.e. the var matrix of a survreg Weibull/loglogistic survival model  
##' @rdname FromDataParam-methods
##' @aliases FromDataParam,missing,character-method
##' @name FromDataParam
##' @export
setMethod("FromDataParam",signature=c(object="missing",type="character"),
  function(object,type,rate,shape,sigma=matrix(c(0,0,0,0),nrow=2)){
         
    if(!type %in% c("weibull","loglogistic")){
      stop("Invalid type")
    }
       
    validate.fromData.arguments(rate,shape,sigma)
    
         
    parameters <- list(rate=rate,shape=shape,sigma=sigma)  
    
    generateParameterFunction <- function(Nsim){
      w.scale <- log(1/parameters$rate)
      w.shape <- 1/parameters$shape
      s <- rmvnorm(Nsim, mean = c(w.scale, log(w.shape)), sigma = parameters$sigma)
      cbind(Id=1:Nsim,
            rate=exp(-s[,1]), #rate (standard parameters) 
            shape=exp(-s[,2])) #shape (standard parameters)
    }
            
   
    conditionalFunction <-  if(type=="weibull") rcweibull else rcloglogistic
    
    new("FromDataSimParam",type=type,parameters=parameters,
        conditionalFunction=conditionalFunction,
        generateParameterFunction=generateParameterFunction)
  }
)

# Check the Weibull/loglogistic rate and shape parameters are sensible
# @param rate proposed rate
# @param shape proposed shape
# @param sigma proposed covariance matrix of -log(rate), -log(shape)
validate.fromData.arguments <- function(rate,shape,sigma){
  if(!is.numeric(rate) || length(rate) != 1 || rate <= 0){
    stop("Invalid rate argument")
  } 
  
  if(!is.numeric(shape) || length(shape) != 1 || shape <= 0){
    stop("Invalid shape argument")
  } 
  
  if(!is.matrix(sigma) || !is.numeric(sigma)){
    stop("Invalid uncertainty matrix argument, it must be a numeric matrix")
  }
  
  if(nrow(sigma)!=2 || ncol(sigma)!=2 || sigma[1,2]!=sigma[2,1]){
    stop("Invalid uncertainty matrix it must be a symmetric 2x2 matrix")
  }
  
  min.val <- 1e-15
  max.val <- 1e15
  
  if(rate < min.val || rate > max.val){
    stop(paste("The rate parameter=",rate," and is outside the range [",min.val,",",max.val,"]",
               " Is this correct",sep=""))
  }
  if(shape < min.val || shape > max.val){
    stop(paste("The shape parameter=",shape," and is outside the range [",min.val,",",max.val,"]",
               " Is this correct?",sep=""))
  }
}
