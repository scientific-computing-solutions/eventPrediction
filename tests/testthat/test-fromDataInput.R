context("dataInput")

data(event.data)

test_that("remove.0.time",{
  expect_error(EventData(data=event.data,
                         subject="subject",
                         site="site",
                         rand.date="randDate",
                         has.event="hasEvent",
                         withdrawn="withdrawn",
                         time="time",
                         remove.0.time=c(TRUE,5)
  ))
  
  expect_error(EventData(data=event.data,
                         subject="subject",
                         site="site",
                         rand.date="randDate",
                         has.event="hasEvent",
                         withdrawn="withdrawn",
                         time="time",
                         remove.0.time="TRUE"
  ))
  
  expect_error(EventData(data=event.data,
                         subject="subject", rand.date="randDate",
                         has.event="hasEvent", withdrawn="withdrawn",
                         time="time", site="site",followup=-50)
  )
  
  expect_error(EventData(data=event.data,
                         subject="subject", rand.date="randDate",
                         has.event="hasEvent", withdrawn="withdrawn",
                         time="time", site="site",followup=c(1,2,3))
  )
  
  e <- event.data
  e$time[1:25] <- NA
  e$time[26:50] <- 0
  
  expect_warning(r.false <- EventData(data=e,
                           subject="subject",
                           site="site",
                           rand.date="randDate",
                           has.event="hasEvent",
                           withdrawn="withdrawn",
                           time="time",
                           remove.0.time=FALSE
  ))

  expect_warning(r.true <- EventData(data=e,
                                    subject="subject",
                                    site="site",
                                    rand.date="randDate",
                                    has.event="hasEvent",
                                    withdrawn="withdrawn",
                                    time="time",
                                    remove.0.time=TRUE
  ))

  
  expect_warning(r.default <- EventData(data=e,
                                     subject="subject",
                                     site="site",
                                     rand.date="randDate",
                                     has.event="hasEvent",
                                     withdrawn="withdrawn",
                                     time="time"                                     
  ))
  
  expect_equal(r.false,r.default)
  expect_equal(nrow(r.false@subject.data),nrow(e))
  expect_equal(e[51:nrow(e),]$subject,r.true@subject.data$subject)
  
})


test_that("Column_names",{
  
  expect_error(EventData(data=event.data,
                         subject="subject",
                         site="site",
                         rand.date="randDate",
                         has.event="hasevent",
                         withdrawn="withdrawn",
                         time="time"))
  
  expect_error(EventData(data=event.data,
                         subject="subject",
                         site="site",
                         rand.date="randdate",
                         has.event="hasEvent",
                         withdrawn="withdrawn",
                         time=time))
  
  expect_error(EventData(data=event.data,
                         subject="sudbject",
                         site="site",
                         rand.date="randDate",
                         has.event="hasEvent",
                         withdrawn="withdrawn",
                         time="time"))
  
  expect_error(EventData(data=event.data,
                         subject="subject",
                         site="sited",
                         rand.date="randDate",
                         has.event="hasEvent",
                         withdrawn="withdrawn",
                         time="time"))
  
  expect_error(EventData(data=event.data,
                         subject="subject",
                         site="sited",
                         rand.date="randDate",
                         has.event="hasEvent",
                         withdrawn="withdrawn",
                         time="time"))
  
  expect_error(EventData(data=event.data,
                         subject="subject",
                         site="site",
                         rand.date="randDate",
                         has.event="hasEvent",
                         withdrawn="withdrawn"
                         ))
  
  expect_error(EventData(data=event.data,
                         subject="subject",
                         site="site",
                         rand.date="randDate",
                         has.event="hasEvent",
                         time="time"
                         ))
  
  expect_error(EventData(data=event.data,
                         subject="subject",
                         site="site",
                         has.event="hasEvent",
                         withdrawn="withdrawn",
                         time="time"
  ))
  
  expect_error(EventData(subject="subject",
                         site="site",
                         rand.date="randDate",
                         has.event="hasEvent",
                         withdrawn="withdrawn",
                         time="time"
  ))
  
  expect_error(EventData(data=event.data,
                         subject="subject",
                         site="site",
                         rand.date="randDate",
                         withdrawn="withdrawn",
                         time="time"
  ))
  
  expect_error(EventData(data=event.data,
                         subject="subject",
                         site="site",
                         rand.date="randDate",
                         withdrawn="withdrawn",
                         time="time",
                         event.type="dfg"
  ))
  
  
})

test_that("Invalid_data_in_df",{
  e <- event.data
  e$hasEvent[5] <- "3"
  
  expect_error(expect_watning(EventData(data=e, subject="subject", rand.date="randDate",
                         has.event="hasEvent", withdrawn="withdrawn",time="time")))
  e <- event.data
  e$withdrawn[15] <- "NO"
  expect_error(EventData(data=e, subject="subject", rand.date="randDate",
                         has.event="hasEvent", withdrawn="withdrawn",time="time"))
  
  e <- event.data
  e$randDate[5] <- NA
  expect_warning(EventData(data=e, subject="subject", rand.date="randDate",
                         has.event="hasEvent", withdrawn="withdrawn",time="time"))
  
  e <- event.data
  e$hasEvent <- rep(0,nrow(e))
  expect_warning(a <- EventData(data=e, subject="subject", rand.date="randDate",
                         has.event="hasEvent", withdrawn="withdrawn",time="time"))
  expect_error(fit(a))
  
  
  e <- event.data
  e$hasEvent <- rep(1,nrow(e))
  expect_error(EventData(data=e, subject="subject", rand.date="randDate",
                         has.event="hasEvent", withdrawn="withdrawn",time="time"))
  
  e <- event.data
  e$withdrawn[1] <- 1
  expect_warning(EventData(data=e, subject="subject", rand.date="randDate",
                           has.event="hasEvent", withdrawn="withdrawn",time="time"))
  
  
  e <- event.data
  e$subject[1] <- 505
  expect_error(EventData(data=e, subject="subject", rand.date="randDate",
                         has.event="hasEvent", withdrawn="withdrawn",time="time"))
  
  e <- event.data
  e$time[1] <- "refd"
  #also get a warning here as code thinks time is factor
  expect_error(expect_warning(EventData(data=e, subject="subject", rand.date="randDate",
                         has.event="hasEvent", withdrawn="withdrawn",time="time")))
  
  e <- event.data
  e$time[1] <- 0
  expect_warning(EventData(data=e, subject="subject", rand.date="randDate",
                           has.event="hasEvent", withdrawn="withdrawn",time="time"))
  
 
  
  e <- event.data
  e$time[1] <- -5
  expect_error(EventData(data=e, subject="subject", rand.date="randDate",
                         has.event="hasEvent", withdrawn="withdrawn",time="time"))

  
  e <- event.data
  e$randDate[1] <- "15/10/2015"
  expect_error(EventData(data=e, subject="subject", rand.date="randDate",
                         has.event="hasEvent", withdrawn="withdrawn",time="time"))
  
  
  e <- event.data
  e$randDate[1] <- "15 Jan 2015"
  expect_error(EventData(data=e, subject="subject", rand.date="randDate",
                         has.event="hasEvent", withdrawn="withdrawn",time="time"))
  
  
  e <- event.data
  e$randDate[1] <- "15-10-31"
  expect_error(EventData(data=e, subject="subject", rand.date="randDate",
                         has.event="hasEvent", withdrawn="withdrawn",time="time"))
  
})


test_that("Derived_Time_Test",{
  
 test.data <- data.frame(Asubject=c(1:8,"",10:12),
             ArandDate=c("01/01/2015","01/01/2015","05/01/2015",
                         "08/01/2015","12/01/2015","15/01/2015",
                         "10/01/2015",          "","06/05/2015",
                         "04/02/2015","08/02/2015","12/03/2015"),
             Ahasevent=c(1,0,0,0,0,0,1,0,0,1,1,1),
             Awithdrawn=c(1,1,1,1,0,0,0,0,0,0,0,0),
             AprogDate=c("","","","10/01/2015","05/04/2015","",
                        "04/12/2015","","","","","05/08/2015"),
             AdthDate=c("04/05/2015","","","","","",
                       "03/05/2015","","","08/02/2015","",""),
             AlastDate=c("","06/06/2015","05/06/2015","",
               "04/04/2015","05/05/2015","","01/05/2015",
               "12/05/2015","05/02/2015","12/02/2015",""),
             AwithdrawnDate=c("01/10/2015","","06/06/2015","12/01/2015",
                             "","05/08/2015","","","","","","06/08/2015")
 )
 
 expect_warning(my.data <- EventData(data=test.data, subject="Asubject", rand.date="ArandDate",
           has.event="Ahasevent", withdrawn="Awithdrawn",
           time=list(last.date="AlastDate",prog.date="AprogDate",withdrawn.date="AwithdrawnDate",dth.date="AdthDate")))
 
 NA.fact <- as.factor(NA)
 
 indat <- data.frame(subject=c(1:7,10:12),
                     rand.date=FixDates(c("01/01/2015","01/01/2015","05/01/2015",
                                 "08/01/2015","12/01/2015","15/01/2015",
                                 "10/01/2015",
                                 "04/02/2015","08/02/2015","12/03/2015")),
                     time=c(124,157,153,5,83,111,114,5,5,147),
                     has.event=c(1,0,0,0,0,0,1,1,1,1),
                     withdrawn=c(0,1,1,1,0,0,0,0,0,0),
                     site=rep(NA,10),
                     event.type=c("Has Event",rep(NA.fact,5),"Has Event","Has Event",
                                  "Has Event","Has Event"),
                     censored.at.follow.up=rep(0,10))
 
  indat$subject <- factor(indat$subject,levels=levels(my.data@subject.data$subject)) 
  
  rownames(indat) <- NULL
  rownames(my.data@subject.data) <- NULL
  expect_equal(indat,my.data@subject.data)
})

test_that("Derived_Time",{
  e <- event.data
  
  
  expect_error(EventData(data=e, subject="subject", rand.date="randDate",
                         has.event="hasEvent", withdrawn="withdrawn",
                         time=list()))
  
  expect_error(EventData(data=e, subject="subject", rand.date="randDate",
                         has.event="hasEvent", withdrawn="withdrawn",
                         time=list(event.date="eD",last.date="lastDate")))
  
  expect_error(EventData(data=e, subject="subject", rand.date="randDate",
                         has.event="hasEvent", withdrawn="withdrawn",
                         time=list(event.date="eventDate")))
  
  expect_error(EventData(data=e, subject="subject", rand.date="randDate",
                         has.event="hasEvent", withdrawn="withdrawn",
                         time=list(last.date="lastDate",eventt.date="eventDate")))
  
  e$wDate <- rep(as.Date(NA),nrow(e))
  e$wDate[1] <-as.Date("2014-01-16")
  
  expect_warning(EventData(data=e, subject="subject", rand.date="randDate",
                         has.event="hasEvent", withdrawn="withdrawn",
                         time=list(last.date="lastDate",event.date="eventDate",withdrawn.date="wDate")))
  
  e <- event.data
  e$wDate <- rep(as.Date(NA),nrow(e))
  e$withdrawn[1] <- 1
  e$hasEvent[1] <- 0
  
  expect_warning(EventData(data=e, subject="subject", rand.date="randDate",
                           has.event="hasEvent", withdrawn="withdrawn",
                           time=list(last.date="lastDate",event.date="eventDate",withdrawn.date="wDate")))
  
})

test_that("CalculateDaysAtRisk",{
  e <- event.data
  e$time <- rep(4,nrow(e))
  my.data <- EventData(data=e,
                       subject="subject",
                       rand.date="randDate",
                       has.event="hasEvent",
                       withdrawn="withdrawn",
                       time="time",
                       site="site")
  
  expect_equal(4*nrow(e),CalculateDaysAtRisk(my.data))
  
})


test_that("Derived_Time_logic",{
  #this is a companion test to Derived_Time_Test above
  e <- event.data

  e$wDate <- rep(NA,nrow(e))
  e$dth.date <- rep(NA,nrow(e))
  e$prog.date <- rep(NA,nrow(e))
    
  e$hasEvent[1] <- 0
  e$eventDate[1] <- NA
  e$lastDate[1] <- "2014-01-14"
  e$hasEvent[2] <- 0
  e$eventDate[2] <- NA
  e$prog.date[2] <- "2014-02-01"
  e$lastDate[2] <- "2014-02-06"
  
  e$hasEvent[3:5] <- 0
  e$withdrawn[3:5] <- 1
  e$eventDate[3:4] <- NA
  e$lastDate[4] <- "2014-02-10"
  e$wDate[4] <- "2014-02-05"
  
  e$lastDate[5] <- "2014-02-20"
  e$wDate[5] <- "2014-03-05"
  
  e$lastDate[6] <- NA
  e$eventDate[7] <- NA
  e$dth.date[7] <- "2014-01-10"
  
  e$prog.date[8] <- "2014-12-03"
  e$eventDate[8] <- "2014-04-04"
  
  e$eventDate[9] <- NA
  e$wDate[9] <- "2014-02-28"
  
  e$withdrawn[10:11] <- 1
  e$eventDate[10:11] <- NA
  e$wdate[11] <- "2014-03-01"
  e$prog.date[11] <- "2014-05-05"
  
  
  expect_warning(a <- EventData(data=e, subject="subject", rand.date="randDate",
            has.event="hasEvent", withdrawn="withdrawn",
            time=list(last.date="lastDate",event.date="eventDate",withdrawn.date="wDate",dth.date="dth.date",
                      prog.date="prog.date")))
  
  expect_equal(c(4,17,45,28,37,41,1,76,26,42,94),a@subject.data$time[1:11])
  
  
})

test_that("prog_dth_warning",{
  
  #check if have both prog and dth date then get warning if dth is before prog
  e <- data.frame(subject=1:3,
                  has.event=c(1,1,0),
                  withdrawn=c(0,0,0),
                  rand.date=rep("2014-01-01",3),
                  last.date=c("","","2015-01-01"),
                  prog.date=c("2015-01-01","2015-01-01",""),
                  dth.date=c("2014-06-06","2015-01-01","")
                  )
  
  expect_warning(EventData(data=e, subject="subject", rand.date="rand.date",
                 has.event="has.event", withdrawn="withdrawn",
                 time=list(last.date="last.date",dth.date="dth.date",prog.date="prog.date")),
                 regexp ="Subjects 1 have progression date after death date. This is invalid and should be fixed" )
  
  
   
})


test_that("followup",{
  e <- data.frame(subject=1:6,
                  randDate=c("2015-01-01","2015-02-01","2015-03-01","2015-04-01","2015-05-01","2015-06-01"),
                  time=(1:6)*10,
                  hasEvent=c(1,1,1,0,1,0),
                  withdrawn=c(0,0,1,0,0,1))
  
  expect_error(EventData(data=e, subject="subject", rand.date="randDate",
            has.event="hasEvent", withdrawn="withdrawn",time="time",followup=0))
  
  expect_error(EventData(data=e, subject="subject", rand.date="randDate",
                         has.event="hasEvent", withdrawn="withdrawn",time="time",followup=c(3,4)))
  
  expect_error(EventData(data=e, subject="subject", rand.date="randDate",
                         has.event="hasEvent", withdrawn="withdrawn",time="time",followup=-10))
  
  #warning for subject 3 having withdrawn and hasEvent=1
  expect_warning(inf.followup <- EventData(data=e, subject="subject", rand.date="randDate",
            has.event="hasEvent", withdrawn="withdrawn",time="time"))
  
  expect_equal(rep(0,6),inf.followup@subject.data$censored.at.follow.up)
  expect_true(is.infinite(inf.followup@followup))
  
  expect_warning(seventy.followup <- EventData(data=e, subject="subject", rand.date="randDate",
                                has.event="hasEvent", withdrawn="withdrawn",time="time",followup=70))
  
  expect_equal(70,seventy.followup@followup)
  expect_equal(inf.followup@subject.data,seventy.followup@subject.data)

  expect_warning(fifteen.followup <- EventData(data=e, subject="subject", rand.date="randDate",
                                               has.event="hasEvent", withdrawn="withdrawn",time="time",followup=15))

  expect_equal(c(1,0,0,0,0,0),fifteen.followup@subject.data$has.event)
  expect_equal(rep(0,6),fifteen.followup@subject.data$withdrawn)
  expect_equal(c(0,rep(1,5)),fifteen.followup@subject.data$censored.at.follow.up)
  
  expect_equal(c(10,rep(15,5)),fifteen.followup@subject.data$time)
  
  expect_warning(twenty.followup <- EventData(data=e, subject="subject", rand.date="randDate",
                                               has.event="hasEvent", withdrawn="withdrawn",time="time",followup=20))
  
  
  expect_equal(c(10,rep(20,5)),twenty.followup@subject.data$time)
  expect_equal(c(1,1,0,0,0,0),twenty.followup@subject.data$has.event)
  
})


test_that("CalculateProgEventTypes",{
  e <- c(0,1,1,1,1,1,1)
  p <- c("01/01/2015","01/01/2015","01/01/2015","01/01/2015","01/01/2015","","")
  d <- c("","","01/01/2014","01/01/2016","01/01/2015","02/01/2015","")  
  
  
  expect_equal(c(NA,"Progression (not death)","Death","Progression (not death)",
                 "Death","Death","Progression (unknown if death)"),
                 CalculateProgEventTypes(e,p,d))
  
  expect_error(CalculateProgEventTypes(c(e,1),p,d))
  expect_error(CalculateProgEventTypes(e,c(p,1),d))
  e[1] <- -3
  expect_error(CalculateProgEventTypes(e,p,d))
  e[1] <- 0
  p[1] <- 23
  expect_error(CalculateProgEventTypes(e,p,d))
  
}) 


test_that("EmptyEventData",{
  e <- EmptyEventData()
  expect_equal(Inf,e@followup)
  expect_equal(0,nrow(e@subject.data))
  e <- EmptyEventData(followup=10)
  expect_equal(10,e@followup)
  expect_error(EmptyEventData(followup=-10))
  expect_error(EmptyEventData(followup=c(1,2,3)))
})



test_that("SubgroupPlotting",{ 
  my.data <- EventData( data=event.data,
                        subject="subject",
                        rand.date="randDate",
                        has.event="hasEvent",
                        withdrawn="withdrawn",
                        subgroup = "arm",
                        time="time" )

  N <- nrow(my.data@subject.data)
  
  # Expect warning (Single level)
  expect_warning( plot( my.data, by.subgroup=TRUE) )

  
  # Should give two parallel but different interceps
  my.data@subject.data$subgroup[1:(N/2)] = 2
  expect_silent( plot( my.data, by.subgroup=TRUE) )
  
  # Should two lines with roughly same interface
  my.data@subject.data$subgroup <- round( runif( N ), 0 ) 
  expect_silent( plot( my.data, by.subgroup=TRUE) )
  
  # Expect warning (Subgroup needs to be defined)
  my.data@subject.data$subgroup[1] <- NA
  expect_warning( plot( my.data, by.subgroup=TRUE ) )
  
  # Expect warning (Subgroup needs to be defined)
  my.data@subject.data$subgroup <- NA
  expect_warning( plot( my.data, by.subgroup=TRUE) )
  
  # Should not warn
  expect_silent( plot( my.data ) )
})
