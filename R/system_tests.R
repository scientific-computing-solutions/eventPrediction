#This file contains a few system level tests 
#which cannot be automated to check before releasing a
#new version of package

#The red and black lines should coincide
#to show from parameters and from data 
#produce the same results
compare.param.data <- function(){
  
  daysinyear <- standarddaysinyear()
   
  study <- SingleArmStudy(N=800,
                 study.duration=36,
                 ctrl.median=3,
                 k=0.5,
                 shape=1.1,
                acc.period=10)
  
  rate <- log(2)^(1/1.1)/3
  rate <- rate*12/daysinyear 
  
  from.param <- predict(study,step.size=0.1)
  
  
  my.data <- EmptyEventData()
  
  my.a <- Generate.Accrual(start.date="2015-01-01",end.date="2015-11-01",k=0.5,deterministic=FALSE)
  
  sim <- simulate(data=my.data,Naccrual=800,accrualGenerator=my.a,
                  SimParams=FromDataParam(type="weibull",rate=rate, shape=1.1),Nsim=500)
  
  rec <- as.numeric(sim@recQuantiles@median-as.Date("2015-01-01"))
  plot(rec,1:length(rec),type="l",xlim=c(0,900),xlab="Time",ylab="N")
  lines(from.param@grid$time*daysinyear/12,from.param@grid$recruit.tot,col="red")
  
  q <- as.numeric(sim@eventQuantiles@median-as.Date("2015-01-01"))
  lines(q,1:length(q),col="black")
  lines(from.param@grid$time*daysinyear/12,from.param@grid$events.tot,col="red")
}

#red and black lines should coincide
#showing conditional Weibull behaves as expected
conditional_Weibull_test <- function(){
  d <- data.frame(subject=1:1000,
                  randDate=rep("2015-01-01",1000),
                  has.event=c(1,rep(0,999)),
                  withdrawn=rep(0,1000),
                  time=rep(50,1000))
  my.data <- EventData(data=d,subject="subject",rand.date="randDate",has.event="has.event",withdrawn="withdrawn",time="time")
  
  results <- simulate(data=my.data, SimParams=FromDataParam(type="weibull",rate=0.01, shape=1.1),Nsim=1000)
 
  dist <- as.numeric(results@eventQuantiles@median-as.Date("2015-01-01")+1)
   
  k <- rweibull(50000,shape=1.1,scale=1/0.01)
  k <- k[k>=50]
  plot(ecdf(k))
  lines(dist,1:1000/1000,col="red")
}

#red and black lines should coincide
#showing conditional Loglogistic behaves as expected
conditional_LogLogistic_test <- function(){
  d <- data.frame(subject=1:1000,
                  randDate=rep("2015-01-01",1000),
                  has.event=c(1,rep(0,999)),
                  withdrawn=rep(0,1000),
                  time=rep(5,1000))
  my.data <- EventData(data=d,subject="subject",rand.date="randDate",has.event="has.event",withdrawn="withdrawn",time="time")
  
  results <- simulate(data=my.data, SimParams=FromDataParam(type="loglogistic",rate=0.1, shape=3),Nsim=1000)
  
  testthat::expect_error(simulate(data=OnlyUseRecTimes(my.data), SimParams=FromDataParam(type="loglogistic",rate=0.1, shape=3),
                        Nsim=1000,HR=0.5,r=1))
  
  dist <- as.numeric(results@eventQuantiles@median-as.Date("2015-01-01")+1)
  
  rloglogis <- function(n,shape,scale){
    u <- runif(n)
    (u/(1-u))^(1/shape)*scale
  }
  
  k <- rloglogis(n=50000,shape=3,scale=1/0.1)
  k <- k[k>=5]
  plot(ecdf(k))
  lines(dist,1:1000/1000,col="red")
}


