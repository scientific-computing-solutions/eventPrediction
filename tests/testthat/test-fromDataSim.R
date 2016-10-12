context("fromDataSim")

test_that("validateArgs",{
  data(event.data)
  my.data <- EventData(data=event.data,
                       subject="subject",
                       rand.date="randDate",
                       has.event="hasEvent",
                       withdrawn="withdrawn",
                       time="time",
                       site="site")
  
  my.fit <- fit(my.data)
  
  expect_error(simulate(my.fit,Nsim=-6))
  expect_error(simulate(my.fit,Nsim=0))
  expect_error(simulate(my.fit,Nsim="sd"))
  expect_error(expect_warning(simulate(my.fit,Nsim=10,seed="we")))
  expect_error(simulate(my.fit,Nsim=10,limit=0.51))
  expect_error(simulate(my.fit,Nsim=10,limit=-0.1))
  expect_error(simulate(my.fit,Nsim=10,limit=c(0.1,0.4)))

  expect_error(simulate(my.fit,Nsim=10,data=my.data@subject.data))
  expect_error(simulate(my.fit,Nsim=10,fix.rate=0.5))
  expect_error(simulate(my.fit,Nsim=10,fix.shape=1.0))
  expect_error(simulate(my.fit,Nsim=10,fix.rate=0.5,fix.shape=-1))
  expect_error(simulate(my.fit,Nsim=10,fix.rate=-0.5,fix.shape=1))
  expect_error(simulate(my.fit,Nsim=10,fix.rate=c(0.5,1.5),fix.shape=1))
  expect_error(simulate(my.fit,Nsim=10,fix.rate=0.5,fix.shape="gt"))
  expect_error(simulate(my.fit,Nsim=10,fix.rate=0.5,fix.shape=c(1,1.01)))
  expect_error(simulate(my.fit,Nsim=10,Naccrual=3))
  
  ag <- Generate.PoissonAccrual(start.date="2014-10-10",rate=1)
  expect_warning(simulate(my.fit,Nsim=10,accrualGenerator=ag))
  expect_warning(simulate(my.fit,Nsim=10,accrualGenerator=ag,Naccrual=0))
  expect_error(simulate(my.fit,Nsim=10,accrualGenerator="e",Naccrual=10))
  expect_error(simulate(my.fit,Nsim=10,accrualGenerator=ag,Naccrual=-4))
  expect_error(simulate(my.fit,Nsim=10,accrualGenerator=ag,Naccrual=c(1,4,5)))
  
  expect_error(simulate(my.fit,Nsim=10,longlagsettings="err"))
  expect_error(simulate(my.fit,Nsim=10,longlagsettings=ag))
  
  expect_error(simulate(my.fit,Nsim=10,HR=4,r=1))
  d <- my.data
  d@subject.data$time <- 0
  d@subject.data$withdrawn <- 0
  d@subject.data$has.event <- 0
  
  expect_error(simulate(my.fit,Nsim=10,data,d,r=1))
  expect_error(simulate(my.fit,Nsim=10,data,d,HR=1))
  expect_error(simulate(my.fit,Nsim=10,data,d,r=1,HR=c(1,2,3)))
  expect_error(simulate(my.fit,Nsim=10,data,d,r=1,HR="er"))
  expect_error(simulate(my.fit,Nsim=10,data,d,r=1,HR=0))
  expect_error(simulate(my.fit,Nsim=10,data,d,r=1,HR=-1))
  expect_error(simulate(my.fit,Nsim=10,data,d,r=0,HR=1))
  expect_error(simulate(my.fit,Nsim=10,data,d,r=-1,HR=1.2))
  expect_error(simulate(my.fit,Nsim=10,data,d,r=c("df",TRUE),HR=1))
  
  
})


test_that("CalculateAccrualTimes",{
  
  myf <- function(N){
    return(rep(as.Date(c("2016-01-01","2016-02-02")) ,N/2))
  }
  
  ac <- new("AccrualGenerator",f=myf,text="t",model="t")
 
  
  rand.dates <- as.Date(c(1:10,25:50,51:100,11:24),origin="2015-01-01")
  
  ans <- CalculateAccrualTimes(0,10,rand.dates,NULL)
  
  expect_equal("matrix",class(ans))
  expect_equal(10,nrow(ans))
  expect_equal(100,ncol(ans))
  
  expect_equal(ans[1,],ans[2,])
  expect_equal(ans[3,],ans[2,])
  expect_equal(16436+c(1:10,25:50,51:100,11:24),ans[2,])
  
  ans <- CalculateAccrualTimes(2,10,rand.dates,ac)
  expect_equal("matrix",class(ans))
  expect_equal(102,ncol(ans))
  expect_equal(10,nrow(ans))

  expect_equal(rep(16801,10),ans[,101])
  expect_equal(rep(16833,10),ans[,102])
  
  expect_equal(16436+c(1:10,25:50,51:100,11:24,365,397),ans[2,])
})

test_that("deterministic_bits",{
  
  set.seed(10)
  
  data(event.data)
  e <- event.data[event.data$hasEvent==1,]
  e <- rbind(e,event.data[700,])
  
  e$randDate[700] <- "2016-01-01"
  
  
  my.data <- EventData(data=e,
                       subject="subject",
                       rand.date="randDate",
                       has.event="hasEvent",
                       withdrawn="withdrawn",
                       time="time",
                       site="site") 
  
  my.fit <- fit(my.data)
    
  results <- simulate(my.fit,Nsim=50,limit=0.25)
  expect_equal(700,length(results@eventQuantiles@median))
  expect_equal(700,length(results@eventQuantiles@upper))
  expect_equal(700,length(results@eventQuantiles@lower))
  expect_equal(700,length(results@recQuantiles@median))
  expect_equal(700,length(results@recQuantiles@upper))
  expect_equal(700,length(results@recQuantiles@lower))
  expect_equal(my.data,results@event.data)
  
  expect_equal(0.25,results@limit)
  expect_equal(0,results@Naccrual)
  
  expect_equal(sort(my.data@subject.data$rand.date),results@recQuantiles@median)
  expect_equal(sort(my.data@subject.data$rand.date),results@recQuantiles@upper)
  expect_equal(sort(my.data@subject.data$rand.date),results@recQuantiles@lower)
  expect_equal(sort(as.Date(e$eventDate[1:699])),results@eventQuantiles@median[1:699])
  expect_equal(sort(as.Date(e$eventDate[1:699])),results@eventQuantiles@lower[1:699])
  expect_equal(sort(as.Date(e$eventDate[1:699])),results@eventQuantiles@upper[1:699])
  
  expect_true(e$time[700] < results@eventQuantiles@median[700])
  
  my.accrual <- Generate.PoissonAccrual(start.date="2015-11-29",rate=1)
  
  expect_warning(results <- simulate(my.fit,Nsim=50,limit=0.25,
                      accrualGenerator=my.accrual,Naccrual=99))
  
  expect_equal(799, length(results@eventQuantiles@median))
  expect_equal(799, length(results@eventQuantiles@lower))
  expect_equal(799, length(results@eventQuantiles@upper))
  
  expect_equal(799,length(results@recQuantiles@median))

  expect_equal(results@eventQuantiles@median,sort(results@eventQuantiles@median))
  expect_equal(results@recQuantiles@median,sort(results@recQuantiles@median))
  expect_true(all(results@eventQuantiles@median <= results@eventQuantiles@upper))
  expect_true(all(results@eventQuantiles@median >= results@eventQuantiles@lower))
})



test_that("GetHRs",{
  
  x <- GetHRs(HR=1,r=1,N=100)
  expect_equal(x,rep(1,100))
  
  set.seed(10)
  x <- GetHRs(HR=0.5,r=3,N=200)
  expect_equal(sort(x),c(rep(0.5,150),rep(1,50)))
  
  set.seed(11)
  y <- GetHRs(HR=0.5,r=3,N=200)
  
  expect_equal(sort(x),sort(y))
  expect_false(all(x==y))
  
  x <- GetHRs(HR=0.5,r=1/3,N=200)
  expect_equal(sort(x),c(rep(0.5,50),rep(1,150)))
})  


test_that("followup",{
  data(event.data)
  my.data <- EventData(data=event.data,
              subject="subject", rand.date="randDate",
              has.event="hasEvent", withdrawn="withdrawn",
              time="time", site="site")
  
  expect_true(is.infinite(my.data@followup))
  
  data.with.follow.up <- EventData(data=event.data,
                      subject="subject", rand.date="randDate",
                      has.event="hasEvent", withdrawn="withdrawn",
                      time="time", site="site",followup=1500)
  
  expect_equal(1500,data.with.follow.up@followup)
  
  my.fit <- fit(my.data)
  expect_warning(simulate(my.fit,Nsim=20,data=data.with.follow.up))
  
  
})

