context("singlesimdetails")

test_that("simplecase",{
  data(event.data)
   
  data <- EventData(data=event.data,
                                   subject="subject", rand.date="randDate",
                                   has.event="hasEvent", withdrawn="withdrawn",
                                   time="time", site="site")
  
  my.fit <- fit(data)  

  res <- simulate(my.fit,Nsim=500,seed=123)
  ssD <- res@singleSimDetails

  #Corret dimensions
  expect_equal(500,ncol(ssD@event.type))
  expect_equal(979,nrow(ssD@event.times))
  
  #No dropouts or withdrawn
  expect_true(all(ssD@event.type==0))  
  
  #First 699 rows are subjects who had events
  expect_equal(1,length(unique(ssD@event.times[1,])))
  expect_equal(1,length(unique(ssD@event.times[698,])))
  expect_equal(1,length(unique(ssD@event.times[699,])))
  
  expect_equal(ssD@event.times[1:699,1],as.numeric(LastDate(data@subject.data)[1:699],origin="1970-01-01") )
  
  #All simulated event times are after Last Date for other subjects
  expect_true(all(ssD@event.times[700:979,1] >= as.numeric(LastDate(data@subject.data)[700:979],origin="1970-01-01") ))
  expect_true(all(ssD@event.times[700:979,41] >= as.numeric(LastDate(data@subject.data)[700:979],origin="1970-01-01") ))
  expect_true(all(ssD@event.times[700:979,401] >= as.numeric(LastDate(data@subject.data)[700:979],origin="1970-01-01") ))
  expect_true(all(ssD@event.times[700:979,500] >= as.numeric(LastDate(data@subject.data)[700:979],origin="1970-01-01") ))
  
})

test_that("subject_withdrawn_and_accrual",{
  
  data(event.data)
  e <- event.data
  e$hasEvent[1:4] <- 0
  e$withdrawn[1:4] <- 1
  
  data <- EventData(data=e,
                    subject="subject", rand.date="randDate",
                    has.event="hasEvent", withdrawn="withdrawn",
                    time="time", site="site")
  
  my.fit <- fit(data)  
  
  Naccrual <- 21
  my.accrual <- Generate.PoissonAccrual("2015-11-29",rate=1)
    
  res <- simulate(my.fit,Nsim=500,seed=123,Naccrual=Naccrual,accrualGenerator=my.accrual)
  ssD <- res@singleSimDetails
  
  
  #Corret dimensions
  expect_equal(500,ncol(ssD@event.type))
  expect_equal(1000,nrow(ssD@event.times))
  
  #correct withdrawn
  expect_true(all(ssD@event.type[5:1000,]==0))  
  expect_true(all(ssD@event.type[1:4,]==1))  
  
  
})

test_that("fixed_follow_up",{
  data(event.data)
  e <- event.data
      
  expect_warning(data <- EventData(data=e,
                    subject="subject", rand.date="randDate",
                    has.event="hasEvent", withdrawn="withdrawn",
                    time="time", site="site",followup=50))
  
  my.fit <- fit(data)  
  
  res <- simulate(my.fit,Nsim=500,seed=123)
  ssD <- res@singleSimDetails
  
  timematrix <- matrix(rep(as.numeric(data@subject.data$rand.date,origin="1970-01-01"),500),ncol=500)
  timematrix <- ssD@event.times - timematrix + 1
  
  expect_true(all(ssD@event.type[700:979,]==2 | timematrix[700:979,]<=50    ))
  expect_true(all(ssD@event.type[700:979,]==0 | timematrix[700:979,]==50    ))
  
})

test_that("dropout",{
  
  my.data <- EmptyEventData()
  
  Naccrual <- 1000
  
 
  f <- function(N){
    return(rep(as.Date("2015-01-01"),N))
  }
  
  my.a <- new("AccrualGenerator",f=f,model="temp",text="temp")
  
  #small enough rate so no events with this seed
  res <- simulate(data=my.data,Nsim=500,seed=123,Naccrual=Naccrual,accrualGenerator=my.a,
                  SimParams=FromDataParam(type="weibull",rate=0.00000002,shape=1),
                  dropout=list(proportion=0.5,time=10))
  ssD <- res@singleSimDetails
  
  expect_true(all(ssD@event.type==1))
    
})


test_that("calculate.at.risk",{
  
  rec.times <- matrix(c(rep(16436,4),rep(16500,4),17000,17025,17050,17025),nrow=3,byrow=TRUE)
  event.times <- matrix(c(rep(16750,4),17000,16750,16750,17250,18000,17400,17600,17125),nrow=3,byrow=TRUE)
  event.type <- matrix(c(rep(0,4),1,2,0,0,1,2,0,0),nrow=3,byrow=TRUE)
  ssD <- SingleSimDetails(event.type,event.times,rec.times)
  
  retVal <- CalculateDaysAtRisk(ssD,c(16000,16436,16437,16500,16750,16751,17020,17030,18000,20000))
  answer <- c(0,1,2,66,566,566.5,701,709.5,1142,1142)
  
  expect_equal(retVal,answer)
  
})
