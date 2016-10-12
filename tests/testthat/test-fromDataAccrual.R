context("test-from-data-accrual")

test_that("Poisson",{
  expect_error(Generate.PoissonAccrual("sd",5))
  expect_error(Generate.PoissonAccrual("2015-10-10",0))
  expect_error(Generate.PoissonAccrual("2015-10-10",-1))
  ac.p <- Generate.PoissonAccrual("2015-01-01",1)
  expect_equal("Poisson process",ac.p@model)
  expect_equal("a poisson process with rate=1 and start date=2015-01-01.",ac.p@text)
  res <- ac.p@f(5000)
  expect_equal(5000,length(res))
  expect_equal("Date",class(res))
  expect_true(all(res >= as.Date("2015-01-01")))
  set.seed(10)
  r <- ac.p@f(10)
  set.seed(20)
  r2 <- ac.p@f(10)
  set.seed(10)
  r3 <- ac.p@f(10)
  expect_equal(r,r3)
  expect_true(any(r!=r2))
})



test_that("Deterministic",{
  expect_error(Generate.Accrual(start.date="2015-01-01",end.date="2015-01-01",k=2))
  expect_error(Generate.Accrual(start.date="2015-10-01",end.date="2015-01-01",k=2))
  expect_error(Generate.Accrual(start.date="2015-01-01",end.date="2015-10-01",k=0))
  expect_error(Generate.Accrual(start.date="2015-01-01",end.date="2015-10-01",k=-2))
  
  ac.d <- Generate.Accrual(start.date="2015-01-01",end.date="2015-12-31",k=1,deterministic=TRUE)
  expect_equal("Power law allocation",ac.d@model)
  expect_equal("a non-stochastic allocation following G(t)=t^k/B^k, where k=1 and B=364 days is the recruitment period [2015-01-01, 2015-12-31].",
               ac.d@text)
  
  
  res <- ac.d@f(365)
  expect_equal(as.character(as.Date("2015-01-01")+0:364), as.character(res))
  
  ac.d <- Generate.Accrual(start.date="2015-01-01",end.date="2015-01-30",k=2,deterministic=TRUE)
  expect_equal(3,sum(ac.d@f(12)<= as.Date("2015-01-16")))
  expect_equal(6,sum(ac.d@f(12)<= as.Date("2015-01-22")))
  
  ac.d <- Generate.Accrual(start.date="2015-01-01",end.date="2015-01-30",k=0.78,deterministic=TRUE)
  set.seed(10)
  r <- ac.d@f(25)
  set.seed(20)
  r2 <- ac.d@f(25) 
  expect_equal(r,r2)
  expect_equal(25,length(r))
    
  expect_error(Generate.Accrual(start.date="2015-01-01",end.date="2015-10-01",k=2,rec.start.date="2015-01-02"))
  ac.d <- Generate.Accrual(start.date="2015-01-01",end.date="2015-12-31",k=1,deterministic=TRUE,rec.start.date="2014-01-01")
  expect_equal(as.character(as.Date("2015-01-01")+0:364), as.character(res))
  
  expect_equal(
  "a non-stochastic allocation following G(t)=t^k/B^k, where k=1 and B=729 days is the recruitment period [2014-01-01, 2015-12-31]. New subjects will be recruited after 2015-01-01.",
  ac.d@text)
  
  ac.d <- Generate.Accrual(start.date="2015-01-01",end.date="2015-01-11",k=0.5,deterministic=TRUE,rec.start.date="2014-12-31")
  
  expect_equal((0.5*(sqrt(11)-1)+1)^2,1+as.numeric(ac.d@f(2)[1]-as.Date("2015-01-01")))
  
  
})

test_that("stochastic_k",{
  ac.s <- Generate.Accrual(start.date="2014-01-01",end.date="2014-12-31",k=1)
  
  out <- "a stochastic allocation following G(t)=t^k/B^k, where k=1 and B=364 days is the recruitment period [2014-01-01, 2014-12-31]."
  expect_equal(out,ac.s@text)
  
  
  set.seed(10)
  r <- ac.s@f(10)
  set.seed(20)
  r2 <- ac.s@f(10)
  set.seed(10)
  r3 <- ac.s@f(10)
  
  expect_equal(10,length(r))
  expect_equal(r,r3)
  expect_true(any(r!=r2))
  
  set.seed(10)
  ac.s <- Generate.Accrual(start.date="2015-01-01",end.date="2015-12-31",k=2,rec.start.date=as.Date("2014-01-01"))
  res <- ac.s@f(10000)
  expect_true(all(res < as.Date("2016-01-01")) && all(res>=as.Date("2015-01-01")) ) 
  
})


test_that("new_accrual",{
  
  NewAccrual <- function(dates){
  
  myf <- function(N){
    sample(x=dates,size=N,replace=TRUE)
  }
  
  new("AccrualGenerator",f=myf,text="new",model="test")
  
  }
  
  
  na <- NewAccrual(as.Date(c("2015-01-01","2014-01-01")))
  d <- as.character(na@f(1000))
  expect_true(all(d %in% c("2015-01-01","2014-01-01")))
  
})

test_that("estimate_k",{
  expect_error(MLestimateK(-8,c(1,2,3)))
  expect_error(MLestimateK(5,c(-0.4,1,1)))
  
  expect_equal(1/4,MLestimateK(exp(4),c(1,1,1)))
  a <- 1/(log(40)-(log(5)+log(9)+log(6)+log(10))/4)
  expect_equal(a,MLestimateK(40,c(5,9,6,10)))
  
  
})