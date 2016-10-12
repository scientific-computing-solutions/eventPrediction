context("simQOutput")

getSomeSimQ <- function(){
  
  median <- as.Date(c("2015-01-01","2015-01-10","2015-01-15","2015-02-01",
                        "2015-02-10","2015-03-01","2015-03-03","2015-04-01"))
  
  lower <- as.Date(c("2014-12-15","2014-12-25","2015-01-02","2015-01-11",
                       "2015-01-28","2015-02-14","2015-02-25","2015-03-13"))
  
  
  upper <- as.Date(c("2015-01-10","2015-01-14","2015-01-23","2015-02-07",
                       "2015-02-19","2015-03-02","2015-03-24","2015-04-05"))
  
  return(SimQOutput(median=median,lower=lower,upper=upper))
}


test_that("Invalid_lengths",{
  
  a <- as.Date("2015-01-10")
  b <- as.Date(c("2015-01-10","2015-01-12"))
  c <- as.Date(c("2015-01-10","2015-01-15"))
  
  expect_error(SimQOutput(upper=a,median=b,lower=c))
  expect_error(SimQOutput(upper=b,median=a,lower=c))
  expect_error(SimQOutput(upper=c,median=b,lower=a))
  
})

test_that("SimQMatrix",{
  
  x <- matrix(c(1:100),ncol=10)
  x[1,1] <- 4
  x[4,1] <- 1
  x[1:2,8] <- c(8,18)
  event.type <- matrix(rep(1,100),ncol=10)
  event.type[,8] <- c(1,1,rep(0,8))
  event.type[4:10,3] <- 0
  
  y <- SimQOutputFromMatrix(x,0.2,10,event.type=event.type,non.inf.event.type=1)
  
  orig <- "1970-01-01"
  medi <- as.Date(c(5.5,15.5,35.5,45.5,55.5,65.5,85.5,95.5,Inf),origin=orig)
  expect_equal(y@median,medi)
  
  lower <- c(quantile(1:10,0.2),quantile(c(8,12:20),0.2),quantile(c(11,18,23,34:40),0.2),
             quantile(c(21,22,33,44:50),0.2),quantile(c(31,32,43,54:60),0.2),quantile(c(41,42,53,64:70),0.2),
             quantile(c(51,52,63,84:90),0.2),quantile(c(61,62,83,94:100),0.2),quantile(c(81,82,93,rep(Inf,7)),0.2))
  lower <- as.Date(lower,origin=orig)
  names(lower) <- NULL
  expect_equal(lower,y@lower)           
  
})


test_that("PredictGivenDates",{
  qs <- getSomeSimQ()
  
  
  ans <- PredictGivenDates(as.Date(c("2015-02-01","2015-03-07","2014-12-13")),qs)
  expect_equal("data.frame",class(ans))
  
  expect_equal(c("time","event","CI_low","CI_high"),colnames(ans))
  expect_equal(as.Date(c("2015-02-01","2015-03-07","2014-12-13")),ans$time)
  
  expect_equal(c(4,7,0),ans$event)
  expect_equal(c(3,6,0),ans$CI_low)
  expect_equal(c(5,7,0),ans$CI_high)
  
})


test_that("PredictGivenTargetEvents",{
  qs <- getSomeSimQ()
    
  expect_error(PredictGivenTargetEvents(c(9,3,5),qs))
  expect_error(PredictGivenTargetEvents(0,qs))
  expect_error(PredictGivenTargetEvents(-1,qs))
  
  ans <- PredictGivenTargetEvents(c(4,2,1,6,8),qs)
  expect_equal("data.frame",class(ans))
  expect_equal(5,nrow(ans))
  
  expect_equal(c(4,2,1,6,8),ans$event)
  expect_equal(as.Date(c("2015-02-01","2015-01-10","2015-01-01","2015-03-01","2015-04-01")),ans$time)
  
  expect_equal(as.Date(c("2015-01-11","2014-12-25","2014-12-15","2015-02-14","2015-03-13")),ans$CI_low)
  expect_equal(as.Date(c("2015-02-07","2015-01-14","2015-01-10","2015-03-02","2015-04-05")),ans$CI_high)
  
})
