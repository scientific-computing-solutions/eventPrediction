# The plotting functions for the AnalysisResults object
# (predict from parameters)

##' @import ggplot2

#' @include study.R results.R eventPrediction_package.R
NULL



##' @param text Text to display be in title, e.g. output
##' from the getSummaryText() function.
##' @param options Use this to customize the output.
##' @param show.separate.arms Logical, if TRUE (and if x is a two arm study) the expected event
##' curves for the separate arms are displayed on the graph, otherwise do not show the curves.
##' @rdname plot-methods
##' @name plot
##' @aliases plot,AnalysisResults,missing-method
##' @export
setMethod( "plot", 
  signature( x = "AnalysisResults",y="missing" ),
  function(x,text=getFromParameterText(x, options = options), 
        options = DisplayOptions(text.width=110), show.title=TRUE,show.separate.arms=!isSingleArm(x@study),
             ylim=NULL) {

    ####### draw plot ######
    
    daysinyear <- standarddaysinyear()
    
    study <- x@study
    if(isSingleArm(study)) show.separate.arms <- FALSE
    
    ts <- x@grid$time
    recruit.tot <- x@grid$recruit.tot
    events1 <- x@grid$events1
    events2 <- x@grid$events2
    events.tot <- x@grid$events.tot
    
    N <- study@N 
    Y <- study@study.duration
    
    oldmar <- par()$mar
    oldlas <- par()$las
    if(show.title){
      chr.pos <- which(unlist(strsplit(text,NULL)) == '\n') 
      chr.count <- length(chr.pos) 
      mar_val <- 1+chr.count
      
    }
    else{
      mar_val <- 0.4
    }
       
    par( mar= c(5,4,mar_val,2)+0.1, las=1 )
    
    plot(range(ts),range(x@grid$recruit.tot),type='n',xlab=" ",ylab=" ", axes=F, ylim=ylim)
    
    #Y axes
    if(is.null(ylim)){ 
      ylim <- c(0,N)
    }
    else{
      N <- ylim[2]-ylim[1]
    }  
        
    rounding <- -floor(log(N,10)-0.3)
    
    startpoint <- round(ylim[1],rounding)
    endpoint <- ylim[2]
    
    axis(side=2,at=(seq(startpoint,endpoint,10^(-rounding))), adj=1, cex.axis=1.00)
    
    mtext("N",side=2,at=((ylim[2]-ylim[1])/2),line=3) 
    
    #X Axis 
    if(options@StartDate=="0") {
      #Numbers
      axis(side=1,at=(0:Y), adj=1, cex.axis=0.85)
    }
    else{
      #Dates  
      startd <- as.Date( options@StartDate, format="%d/%m/%Y")
      date2  <- as.Date(startd+(0:ceiling(ts[length(ts)]))*daysinyear/12, format="%d/%m/%Y")
      date  <- format(date2, format="%b %Y")
              
      every <- floor(length(date)/50)
      ats <- seq(0,length(date)-1,every+1)
      date <- date[c(TRUE,rep(FALSE,every))]
            
      axis(side=1,at=ats, labels=as.character(date),adj=1,cex.axis=0.85, las=2)
    }
    
    
    #X Axes Labels
    if( options@StartDate=="0" ) {
      mtext('Time (Months)',side=1,at=(Y/2),line=2)
    } else{
      #Dates - not outputting  
      #mtext('Time',side=1,at=-1.5,line=2)
    }
    
    #Add Lines
    box()
    lines(ts,recruit.tot,lty=1,col="black",lwd=2)
    if(show.separate.arms ){  
      lines(ts,events2,lty=8,col="red",lwd=2)
      lines(ts,events1,lty=8,col="blue",lwd=2)
    }
    lines(ts,events.tot,lty=8,col="black",lwd=2)
    if( nrow(x@critical.data)>0 ) abline( v = x@critical.data$time )
    
    if( nrow(x@predict.data) > 0  )  {
      abline(v = x@predict.data$time, lty = 2)
      if(show.separate.arms){ 
        abline(h = x@predict.data$events2,lty=2,col="red")
        abline(h = x@predict.data$events1,lty=2, col="blue")
      }
    }
    
    if(show.separate.arms){ 
      legend( "topleft", c("Recruitment", "Events : Total", "Events : Control", "Events : Experimental" ),
              col=c( "black", "black", "blue", "red" ),
              lty=c(1,8,8,8),text.col=c("black","black","blue","red"), lwd=c(2,2,2,2), bty="n")
    }
    else{
      legend( "topleft", c("Recruitment", "Events : Total" ),
              col=c( "black", "black" ),
              lty=c(1,8),text.col=c("black","black"), lwd=c(2,2), bty="n")  
    }
    
    
    ####### Add in Summary Info and Results ######
    if( show.title==TRUE ) {
      mtext(text,side=3,cex=1,adj=0)
    } 
    par( mar=oldmar, las=oldlas )
    
})

