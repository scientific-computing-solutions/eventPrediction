#File which outputs the summary text for the
#predict from parameters part of the package
#In future refactoring away this summary text
#(maybe replacing with a table of output) will
#improve maintainability?

##' @include common.R
NULL

# Creates the text to be output when calling
# summary of an AnalysisResults object
# 
# @param object An AnalysisResults object
# @param options DisplayOptions
# @return The summary text 
getFromParameterText <-function(object,options){
  study <- object@study
  daysinyear <- standarddaysinyear()
  ####### generate summary text about assumptions ######
  ####  Start date   ######
  startd <- as.Date( options@StartDate, format="%d/%m/%Y")
  date2  <- as.Date(startd+(0:length(t))*daysinyear/12, format="%d/%m/%Y")
  date   <- format(date2, format="%d/%m/%Y")
  date3  <- format(date2, format="%b %Y")
  
  recruit.txt <- ''
  if( options@Trecruit ) {
    recruit.txt <- paste0( study@N,' patients recruited, ')
    if(!is.infinite(study@followup)){
      recruit.txt <- paste0(recruit.txt,"and are followed for ",study@followup," ",options@Time,", ")
    }  
  }
  
  ratio.txt <- ''
  if( options@Tratio && !isSingleArm(study) ) {
    ratio.txt <- paste0('ratio nE/nC=',study@r,', ')
  }
  
  acc.txt <- ''
  if( options@Tacc) {
    if( study@k==1 ) {
      acc.txt <- paste0( study@acc.period,' ', options@Time,' accrual (uniform accrual, k=', study@k, ').' )
    }  
    else{ 
      acc.txt <- paste0( study@acc.period, ' ', options@Time, ' accrual (non-uniform accrual, k=', study@k, ').' )
    }  
  }
  
  
  median.txt <- ''
  hr.txt <- ''
  
  if(!isNullLag(study@lag.settings) ){
    
    if( options@Tmedian) {
      median.txt <- paste0('Lag time: T=', study@lag.settings@Lag.T, " ", options@Time, ', ', options@Control, 
                           ' for [0,T] ', study@lag.settings@ctrlSpec@text, 
                           " ", options@Time,' and for [T,S] ', options@Control," " ,
                           study@ctrlSpec@text, " ", options@Time, "." )
    
      
    }
    
    if( options@Thr && !isSingleArm(study) ) {          
      hr.txt <- paste0('HR([0,T])=', study@lag.settings@L.HazardRatio, ' and HR([T,S])=', 
                       study@HR, ', which gives an average HR=',round( object@av.hr, digits=2 ),'. ')
    }
          
  }
  else{
    
    if( options@Tmedian) {
      median.txt <- paste0( options@Control," ", study@ctrlSpec@text," ", options@Time,' (lambda=',round( object@sfns[[1]]@lambda, digits=2 ),'). ')
      if(!isSingleArm(study))                      
        median.txt <- paste0(median.txt, options@Exp,' ', study@ctrlSpec@experimentaltext(study@HR,study@shape),
                             " ",options@Time,' (lambda=', round( object@sfns[[2]]@lambda, digits=2 ),').')
    }
    
    if( options@Thr && !isSingleArm(study)) {
      hr.txt <- paste0('HR(', options@Exp, ':', options@Control, ')=', round( study@HR, digits=2 ),', ')
    }
  }     
  
  
  if(options@Tmedian){
    if(object@study@shape!=1) 
      median.txt <- paste(median.txt," Weibull survival function shape=",object@study@shape,".",sep="")
    else{
      median.txt <- paste(median.txt," Exponential survival function.",sep="") 
    }
  }
  
  
  dropout.txt <- ""
  if(options@Dropout){
    if(!is.infinite(object@study@dropout[[1]]@median)){
      dropout.txt <- paste(options@Control,"dropout:",object@study@dropout[[1]]@text,options@Time)
    }
    
    if(!isSingleArm(study) && !is.infinite(object@study@dropout[[2]]@median)){
      dropout.txt <- paste(dropout.txt,"and",options@Exp ,"dropout:",object@study@dropout[[2]]@text,options@Time,"both arms")  
    }
    
    if(dropout.txt!=""){
      if(object@study@dropout.shape==1){
        dropout.txt <- paste(dropout.txt," using exponential dropout rate.")
      }
      else{
        dropout.txt <- paste(dropout.txt," using Weibull dropout with shape=",object@study@dropout.shape,".",sep="")
      }
    }
  
  } 
   
  
  crithr.txt <- ''
  if( options@Tcrithr && !isSingleArm(study)) {
    if(isNullLag(study@lag.settings)){
      crithr.txt <- paste0('critical HR value=',floor( object@critical.HR*100)/100,', ')
    }
    else{
      crithr.txt <- paste0('For a study with no lag and this HR: critical HR value=',floor( object@critical.HR*100)/100,', ')  
    }
  }
  
  if(isSingleArm(study)){
    param.txt <- ''
  }else if( study@two.sided )  { 
    param.txt <- paste0('alpha(2-sided)=', study@alpha*100,'%, power=', round(study@power*100,digits=0),'%,')
  }   
  else{
    param.txt <- paste0('alpha(1-sided)=', study@alpha*100,'%, power=', round(study@power*100,digits=0),'%,')
  }
  
  line1.txt <- paste0( recruit.txt, ratio.txt, acc.txt )
  line3.txt <- paste0( hr.txt, crithr.txt, param.txt )
  
  ####### generate text about events required and when reached ######
  if( nrow(object@critical.data)>0 ) {
    at.txt <- paste( 'expected at time', floor(object@critical.data[1,"time"]*10)/10, options@Time )
    at.txt <- paste(at.txt,' (',options@Exp,'/',options@Control,': ', trunc( object@critical.data[1,"events2"] ),'/',
                    trunc( object@critical.data[1,"events1"]),').',sep="")
  } 
  else { 
    at.txt <- paste( 'not reached by time ', study@study.duration,".",sep="")
  }
  
  if(isSingleArm(study)){
    events1.txt <- ''
  }
  else{
    elag.txt <- if(isNullLag(study@lag.settings)) '' else "and using the given lag settings: "
    events1.txt <- paste( ceiling( object@critical.events.req ),' events required ',elag.txt,at.txt,sep='')
  }
  
  
  summary1.txt <- paste(line1.txt, median.txt,dropout.txt)
  summary2.txt <- paste(line3.txt, events1.txt)
  summary3.txt <- GetPredictionText(object,options,study@power,isSingleArm(study)) 
  
  return( AddLineBreaks( paste(summary1.txt, summary2.txt, summary3.txt ),text.width=options@text.width)) 
}


# Generates the text for the Analysis results concerning the number of
# events occurring at user chosen time.pred times
# 
# @param results A results object
# @param options A DisplayOptions object
# @param study.power The power of the study
# @param isSingleArm Logical, True if study is single arm
# @return The text concerning the number of events occurring at user chosen time.pred times
GetPredictionText <- function(results,options,study.power,isSingleArm){
  p.df <- results@predict.data
  if(nrow(p.df)==0) return("")
  
  events2.txt <- ""
  
  if(nrow(p.df)>1){
    l <- "("
    r <- ")"
  }
  else{
    l <- ""
    r <- ""
  }
    
    
  output_times <- paste(l,paste(round(p.df$time,digits=1),collapse=", "),r,sep="")
    
  pred_events <- floor( p.df[,"events1"] ) + floor(p.df[,"events2"] )
  pred_events <- paste(l,paste(pred_events,collapse=", "),r,sep="")
        
  if("at.risk1" %in% colnames(p.df)){
    atrisk.value <- round(options@atRiskConversion*(p.df[,"at.risk1"] +p.df[,"at.risk2"]),digits=2)
    atrisk.value <- paste(l,paste(atrisk.value,collapse=", "),r,sep="")
  }
  else{
    atrisk.value <- NA
  }
    
  control_events <- paste(l,paste(trunc( p.df[,"events1"] ),collapse=", "),r,sep="") 
  exp_events <- paste(l,paste(trunc( p.df[,"events2"] ),collapse=", "),r,sep="") 
    
  events2.txt <- paste0("At ", output_times,' ',options@Time," the predicted number of events is ", 
                        pred_events)
    
  if(!isSingleArm){
    events2.txt <- paste0(events2.txt,' [',options@Exp,'/',options@Control,': ',exp_events,'/',control_events,']')     
  }
    
  if(options@ShowRec){
    recs <- floor(p.df[,"recruit.tot"])
    recs <- paste(l,paste(recs,collapse=", "),r,sep="")
    events2.txt <- paste0(events2.txt," and the expected number of subjects recruited is ", recs,sep="")
  }
    
  if(options@showatRisk && !is.na(atrisk.value)){
    events2.txt <- paste0(events2.txt," with a total of ", atrisk.value ," patient ", options@atRiskTime, " at risk",sep="")
  }
  
  paste(events2.txt,".",sep="")

}

