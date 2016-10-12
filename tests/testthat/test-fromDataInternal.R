context("fromDataInternal")


test_that("myRound.Date",{
  
  ans <- myRound.Date(as.Date(c("2015-01-16","2015-01-16","2015-01-16")),round.method="toMonths")
  expect_equal(rep("Jan 2015",3),ans)
  ans <- myRound.Date(as.Date(c("2015-01-16","2015-01-16","2015-01-16")),round.method="none")
  expect_equal(rep(as.Date("2015-01-16"),3),ans)
  ans <- myRound.Date(as.Date(c("2015-01-15","2015-01-15","2015-01-15")),round.method="toMonths")
  expect_equal(c("Dec 2014","Jan 2015","Jan 2015"),ans)
  ans <- myRound.Date(as.Date(c("2015-01-17","2015-01-17","2015-01-17")),round.method="toMonths")
  expect_equal(c("Jan 2015","Jan 2015","Feb 2015"),ans)
  
})

test_that("FixDates",{
  
  expect_error(FixDates(16436))
  a <- c(as.Date("2015-01-01"),as.Date("2014-12-10"))
  
  expect_equal(a,FixDates(a))
  expect_equal(as.Date(c(NA,NA)),FixDates(c(NA,NA)))
  expect_equal(as.Date(c(NA,NA,"2015-01-01")),FixDates(c("",NA,"2015-01-01")))
  expect_equal(c(as.Date("2015-01-01"),NA,NA),FixDates(c(as.Date("2015-01-01"),NA,NA)))
  
  expect_equal(a,FixDates(c("01/01/2015","10/12/2014")))
  expect_equal(a,FixDates(c("01 January 2015","10 Dec 2014")))
  
  expect_error(FixDates(c("01 January 2015","10/12/2014","15/05/2016")))
  expect_error(FixDates(c("01 January 2015","2014-10-12","05 Mar 2013")))

  expect_equal(as.Date("2015-01-01"),FixDates("2015-01-01"))
  
  expect_error(FixDates("05 July 15"))
  expect_error(FixDates("31 June 2015"))
  
  expect_error(FixDates(c(" ","2015-01-01")))
  
})


test_that("internal.Date",{
  
  times <- c(0,1,5,6,10)
  rand.date <- as.Date(c("2015-06-06","2014-01-01","2015-01-01","2015-06-05","2014-10-10"))
  
  expect_equal(as.Date(c("2015-06-06","2014-01-01","2015-01-05","2015-06-10","2014-10-19"))
      ,internal.Date(times,rand.date))
  
  
})


test_that("LastDate",{
  times <- c(0,1,5,6,10)
  rand.date <- as.Date(c("2015-06-06","2014-01-01","2015-01-01","2015-06-05","2014-10-10"))
  
  df <- data.frame(time=times,rand.date=rand.date)
  expect_equal(as.Date(c("2015-06-06","2014-01-01","2015-01-05","2015-06-10","2014-10-19")),LastDate(df))
  
})


