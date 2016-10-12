#This file contains the various constructors used to 
#create Study objects

#' @include study.R
NULL

##' Create a \code{Study} object for a single arm trial
##' @inheritParams Study
##' @return A \code{Study} object
##' @export
SingleArmStudy <- function(N, study.duration,ctrl.median,k,acc.period,shape=1,dropout=NULL,lag.settings=NullLag()){
  NAn <- as.numeric(NA)
  
  dropoutctrlSpec <- CtrlSpecFromList(dropout,eventtext="(in absence of events) would drop out by",1)
  dropout.shape <- if(is.null(dropout) || is.null(dropout$shape)) 1 else dropout$shape
    
  new("Study",alpha=NAn,power=NAn,HR=NAn,
      r=0,N=N, study.duration=study.duration,ctrlSpec=CtrlSpecfromMedian(ctrl.median),
      k=k,acc.period=acc.period,two.sided=FALSE,shape=shape,followup=Inf,type="Oncology",dropout=dropoutctrlSpec,
      dropout.shape=dropout.shape,lag.settings=lag.settings)  
} 

##' Constructor for a Study 
##' @param HR Hazard ratio to be detected
##' @param alpha Significance level [0,1] (see also two-sided indicator)
##' @param power Power [0,1]
##' @param r Control:Experimental subject balance (1:r), i.e. nE/nC=r. r=1 corresponds to equally 
##' many subjects in both arms. 2 means we have twice the number of subjects in the experimental arm.
##' @param N Number of subjects to be recruit (integer)
##' @param study.duration Number of months the study will be going.
##' @param ctrl.median Median time to an event in the control group.
##' @param k non-uniformity of accrual (integer, 1=uniform). Non-uniform accrual is allowed for 
##' using the following distribution for the probability of a patient entering the trial at time \eqn{b} 
##' within the accrual period \eqn{[0,B]}: \eqn{F(b)=b_k/B_k}; \eqn{f(b)=k b_{k-1}/B_k} where \eqn{k} is the 
##' measure of non-uniformity (\eqn{k>0}). \eqn{k=1} indicates uniform accrual. This implies that during 
##' the first half of the accrual period, \eqn{1/2^k} of the patients will be recruited. Half of the patients 
##' will be recruited by time \eqn{B/2^{1/k}}. 
##' @param acc.period Accrual time.
##' @param two.sided If TRUE, two sided test will be used (i.e. alpha/2).
##' @param shape The Weibull shape parameter 
##' @param dropout if subjects drop out in study (due to competing risks not as there is a finite follow up time)
##' then this argument should contain a list with proportion and time and optionally shape i.e.
##' \code{dropout=list(proportion=0.03,time=12,shape=1.2)} meaning in the absence of events 3% of subjects
##' will have dropped out after 12 months with a Weibull hazard rate with shape=1.2. If shape is not included then 
##' it defaults to 1 (exponential rate). If dropout is NULL then no subjects will drop out
##' @param lag.settings The \code{LaggedEffect} object describing any lag effect for the study 
##' @export
Study <- function(alpha, power, HR, r, N, study.duration, 
                  ctrl.median, k, acc.period, two.sided,shape=1,dropout=NULL,
                  lag.settings=NullLag()){
  
  
  dropoutctrlSpec <- CtrlSpecFromList(dropout,eventtext="(in absence of events) would drop out by",2)
  dropout.shape <- if(is.null(dropout) || is.null(dropout$shape)) 1 else dropout$shape
  
  new("Study",alpha=alpha,power=power,HR=HR,
      r=r,N=N, study.duration=study.duration,ctrlSpec=CtrlSpecfromMedian(ctrl.median),
      k=k,acc.period=acc.period,two.sided=two.sided,shape=shape,followup=Inf,type="Oncology",
      dropout=dropoutctrlSpec,dropout.shape=dropout.shape,lag.settings=lag.settings)  
}

##' Create a \code{Study} object for a CRGI type study
##' @inheritParams Study
##' @param ctrl.time The time at which \code{ctrl.proportion} of the control group have had an event.
##' Used to set the event rate 
##' @param ctrl.proportion The proportion of control arm subjects who have had an event by time \code{time} 
##' @param followup The follow up time for each subject
##' @return A \code{Study} object  
##' @export
CRGIStudy <- function(alpha, power, HR, r, N, study.duration, 
                      ctrl.time,ctrl.proportion, k, acc.period, two.sided,shape=1,followup,dropout=NULL,
                      lag.settings=NullLag()){

  dropoutctrlSpec <- CtrlSpecFromList(dropout,eventtext="(in absence of events) would drop out by",2)
  dropout.shape <- if(is.null(dropout) || is.null(dropout$shape)) 1 else dropout$shape
  
  new("Study",alpha=alpha,power=power,HR=HR,
      r=r,N=N, study.duration=study.duration,
      ctrlSpec=CtrlSpecfromProportion(time=ctrl.time,proportion.had.event=ctrl.proportion,shape=shape),
      k=k,acc.period=acc.period,two.sided=two.sided,shape=shape,followup=followup,type="CRGI",dropout=dropoutctrlSpec,
      dropout.shape=dropout.shape,lag.settings=lag.settings)
  
}


##' Create a \code{Study} object for a CRGI type single arm study
##' @inheritParams Study
##' @param ctrl.time The time at which \code{ctrl.proportion} of subjects have had an event.
##' Used to set the event rate 
##' @param ctrl.proportion The proportion of subjects who have had an event by time \code{time} 
##' @param followup The follow up time for each subject
##' @return A \code{Study} object 
##' @export
SingleArmCRGIStudy <- function(N, study.duration,ctrl.time,ctrl.proportion,k,acc.period,shape=1,followup,
                               dropout=NULL,lag.settings=NullLag()){
  NAn <- as.numeric(NA)
  
  dropoutctrlSpec <- CtrlSpecFromList(dropout,eventtext="(in absence of events) would drop out by",1)
  dropout.shape <- if(is.null(dropout) || is.null(dropout$shape)) 1 else dropout$shape
    
  new("Study",alpha=NAn,power=NAn,HR=NAn,
      r=0,N=N, study.duration=study.duration,
      ctrlSpec=CtrlSpecfromProportion(time=ctrl.time,proportion.had.event=ctrl.proportion,shape=shape),
      k=k,acc.period=acc.period,two.sided=FALSE,shape=shape,followup=followup,type="CRGI",dropout=dropoutctrlSpec,
      dropout.shape=dropout.shape,lag.settings=lag.settings)  
} 
