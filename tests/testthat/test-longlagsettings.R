context("longlagsettings")
test_that("LongLagSettings",{
  
  expect_error(LongLagSettings(ndays=-1))
  expect_error(LongLagSettings(ndays=c(1,4)))
  expect_error(LongLagSettings(ndays=4,visitschedule=-2))
  expect_error(LongLagSettings(ndays=4,visitschedule=2,analysis.date="dsc"))
})


test_that("DealWithReportingLag",{
  
  d <- data.frame(subject=1:10,
                  rand.date=rep("2015-01-01",10),
                  has.event=c(1,rep(0,9)),
                  withdrawn=c(0,1,rep(0,8)),
                  time=seq(10,100,10))
                  
                  
  my.data <- EventData(data=d,
                       subject="subject",
                       rand.date="rand.date",
                       has.event="has.event",
                       withdrawn="withdrawn",
                       time="time")                
  
  indat <- my.data@subject.data
  expect_equal(indat,DealWithReportingLag(indat,longlagsettings=NULL))
  
  lls <- LongLagSettings(ndays=20,toWithdraw=TRUE)
  new.indat <- DealWithReportingLag(indat,lls)
  
  expect_equal(seq(10,100,10),new.indat$time)
  expect_equal(c(1,rep(0,9)),new.indat$has.event)
  expect_equal(c(0,rep(1,6),0,0,0),new.indat$withdrawn)
  
  lls <- LongLagSettings(ndays=60,toWithdraw=FALSE,analysis.date ="2015-05-01")
  new.indat <-  DealWithReportingLag(indat,lls)
  
  expect_equal(c(10,20,rep(121,4),70,80,90,100),new.indat$time)
  expect_equal(c(1,rep(0,9)),new.indat$has.event)
  expect_equal(c(0,1,rep(0,8)),new.indat$withdrawn)
  
  
})