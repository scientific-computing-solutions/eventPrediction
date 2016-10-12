context("fromDataDiagnostics")

test_that("average_rec",{
  x <- as.numeric(as.Date("2014-01-01"))
  y <- as.numeric(as.Date("2014-01-02"))
  expect_equal(50, average.rec(100,x,y))
  expect_equal(100, average.rec(100,x,x))
  z <- 364+x
  expect_equal(0.6, average.rec(219,x,z))
})

test_that("LaggedSubjects",{
  data(event.data)

  e2 <- event.data
  e2$withdrawn[970] <- 1
  
  e <- EventData(data=e2,
                       subject="subject",
                       rand.date="randDate",
                       has.event="hasEvent",
                       withdrawn="withdrawn",
                       time="time",
                       site="site")
  
  f <- GetLaggedSubjects(e@subject.data,as.Date("2013-01-01"))
  expect_equal(0,nrow(f))
  
  f <- GetLaggedSubjects(e@subject.data,as.Date("2017-10-10"))
  expect_equal(subset(e@subject.data,withdrawn==0& has.event==0),f)
  
  f <- GetLaggedSubjects(e@subject.data,as.Date("2015-11-26"))
  expect_equal(c(298,344,313,794),f$subject)
})

test_that("Site_censor_Information",{
  data(event.data)
  
  e2 <- event.data
  e2$withdrawn[970] <- 1
  
  e <- EventData(data=e2,
                 subject="subject",
                 rand.date="randDate",
                 has.event="hasEvent",
                 withdrawn="withdrawn",
                 time="time")
  
  expect_error(siteInformation(e,ndays=as.Date("2014-10-01")))
  expect_error(siteInformation(e,ndays=-1))
  expect_error(siteInformation(e,ndays=10,analysis.date=34))
  expect_error(siteInformation(e,ndays=10))
  
  e <- EventData(data=e2,
                 subject="subject",
                 rand.date="randDate",
                 has.event="hasEvent",
                 withdrawn="withdrawn",
                 time="time",site="site")
  
  s <- siteInformation(e,ndays=10)
  expect_equal("data.frame",class(s))
  expect_equal(as.factor(c(4,3,8)),s$site)
  expect_equal(c(2,1,1),s$count)
  
  s <- siteInformation(e,ndays=10,analysis.date=as.Date("2015-11-28"))
  s2 <- siteInformation(e,ndays=10,analysis.date="2015-11-28")
  s3 <- siteInformation(e,ndays=11,analysis.date="2015-11-29")
      
  expect_equal(s,s2)
  expect_equal(s,s3)
  
  s4 <- siteInformation(e,ndays=10,analysis.date=as.Date("2015-11-28"))
  s5 <- siteInformation(e,ndays=10,analysis.date=NULL)
  expect_equal(s4,s5)
  
  
  expect_error(censorInformation(e,censor.date=2013-10-10))
  s <- censorInformation(e,censor.date=as.Date("2013-10-10"))             
  expect_equal("data.frame",class(s))
  expect_equal(0,nrow(s))
  
  s <- censorInformation(e,censor.date=as.Date("2017-10-10"))
  expect_equal(sort(s$subject),sort(e@subject.data$subject[e@subject.data$withdrawn==0&e@subject.data$censored.at.follow.up==0&
                                                             e@subject.data$has.event==0]))
  
  s <- censorInformation(e,censor.date=as.Date("2015-11-25"))
  expect_equal(c(313,344,298,794),s$subject)
  
  s1 <- censorInformation(e,censor.date="2015-11-28")  
  s2 <- censorInformation(e,censor.date=NULL)  
  expect_equal(s1,s2)
  
})


test_that("CutData",{
  data(event.data)
  e2 <- event.data
  
  e2$withdrawn[974:979] <- 1
  
  
  expect_warning(my.data <- EventData(data=e2,
                 subject="subject",
                 rand.date="randDate",
                 has.event="hasEvent",
                 withdrawn="withdrawn",
                 time="time",
                 followup=500))
  
  expect_error(CutData(my.data,"2013-12-31"))
  expect_that(suppressWarnings(cut1 <- CutData(my.data,"2014-01-01")), not(throws_error()))
  expect_equal(cut1@subject.data$subject,event.data$subject[event.data$randDate <= as.Date("2014-01-01")]) 

  expect_warning( CutData(my.data,"2016-01-01"))
  
  cut2 <- CutData(my.data,"2015-01-01")
  expect_true(all(eventPrediction:::LastDate(cut2@subject.data) <= as.Date("2015-01-01")))
  
  expect_equal(rep(0,6),tail(cut2@subject.data$withdrawn,6)) #last 6 subjects were not cut but are no longer withdrawn
  
  expect_equal(0,cut2@subject.data[cut2@subject.data$subject==251,"has.event"])
  expect_equal(as.Date("2015-01-01"),eventPrediction:::LastDate(cut2@subject.data)[cut2@subject.data$subject==251])  
})


test_that("event.type",{
  data(event.data)
  
  e2 <- event.data
  e2$eventType[1] <- ""
  #Warning if missing individual event.type
  expect_warning(
    x <- EventData(data=e2,
              subject="subject",
              rand.date="randDate",
              has.event="hasEvent",
              withdrawn="withdrawn",
              time="time",
              event.type="eventType")
  )
  
  expect_equal(c("Event type 1","Event type 2","Had Event"),levels(x@subject.data$event.type) )
  
  
  e2 <- event.data
  e2$eventType[979] <- "Had Event"
  expect_warning(
    x <- EventData(data=e2,
              subject="subject",
              rand.date="randDate",
              has.event="hasEvent",
              withdrawn="withdrawn",
              time="time",
              event.type="eventType")
  )
  
  expect_equal(c("Event type 1","Event type 2"),levels(x@subject.data$event.type) )
  
  expect_warning(z <- CutData(x,"15/01/2014"))
  expect_equal(character(0),levels(z@subject.data$event.type))
  
  z <- CutData(x,"22/01/2014")
  expect_equal(c("Event type 1"),levels(z@subject.data$event.type))
  
  
  y <- EventData(data=e2,
                 subject="subject",
                 rand.date="randDate",
                 has.event="hasEvent",
                 withdrawn="withdrawn",
                 time="time")
  expect_true(all(y@subject.data$has.event==1 | is.na(y@subject.data$event.type) ))
  expect_true(all(y@subject.data$has.event==0 | y@subject.data$event.type=="Has Event" ))
  
  
  
})
