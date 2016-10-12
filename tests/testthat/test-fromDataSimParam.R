context("fromDataSimParam")

test_that("FromDataParam_rate_shape_input",{
  
  expect_error(FromDataParam(type="c",rate=1,shape=1))
  expect_error(FromDataParam(type="weibull",rate=1e-25,shape=1))
  expect_error(FromDataParam(type="weibull",rate=1,shape=c(1,2,3)))
  expect_error(FromDataParam(type="weibull",rate=1,shape=-1))
  expect_error(FromDataParam(type="weibull",rate=1,sigma=matrix(c(0,0,0,0),ncol=2)))
  expect_error(FromDataParam(type="weibull",rate=1,shape=1,sigma=matrix(c(0,0,0,0),ncol=1)))
  expect_error(FromDataParam(type="weibull",rate=1,shape=1,sigma=matrix(c("a","b","a","c"),ncol=2)))
  expect_error(FromDataParam(type="weibull",rate=1,shape=1,sigma=matrix(c(0,4,5,0),ncol=2)))

  x <- FromDataParam(type="weibull",rate=1.4,shape=1)  
  expect_equal("weibull",x@type)
  expect_equal(1.4,x@parameters$rate)
  expect_equal(1,x@parameters$shape)
  expect_equal(matrix(c(0,0,0,0),ncol=2),x@parameters$sigma)
  
  m <- matrix(c(1,5.6,5.6,8),ncol=2)
  x <- FromDataParam(type="weibull",rate=1.4,shape=1,sigma=m)
  expect_equal(m,x@parameters$sigma)
  
  
})

test_that("Weibull_generate_sim_params",{
  
  x <- FromDataParam(type="weibull",rate=1.4,shape=1) 
  
  gPF <- x@generateParameterFunction(10)
  expect_true(is.matrix(gPF))
  expect_equal(10,nrow(gPF))
  expect_equal(c("Id","rate","shape"),colnames(gPF))
  expect_equal(1:10,gPF[,1])
  expect_equal(rep(1.4,10),gPF[,2])
  expect_equal(rep(1,10),gPF[,3])
  
  x <- FromDataParam(type="weibull",rate=4,shape=1,sigma=matrix(c(1,0,0,1),nrow=2)) 
  gPF <- x@generateParameterFunction(8)
  expect_true(all(rep(1.4,8) != gPF[,2]))
})


test_that("weibull_generate_times",{
  x <- FromDataParam(type="weibull",rate=1.4,shape=1.2) 
  
  gPF <- x@generateParameterFunction(1)
  
  set.seed(10)
  times <- x@conditionalFunction(c(1,2,3,4,5),params=gPF[1,],HR=c(1,1,1,1,1) )
  expect_equal(5,length(times))
  expect_true(all( 1:5< times))
  
  set.seed(10)
  times.HR <- x@conditionalFunction(c(1,2,3,4,5),params=gPF[1,],HR=c(1,0.5,0.5,0.5,0.5) )
  expect_equal(times[1],times.HR[1])
  expect_true(all(times <= times.HR))
})

test_that("FromDataSurvreg",{
 
  df <- data.frame(time=rweibull(100,shape=1.1,scale=1.1),has.event=rep(1,100))
  
  model <- survreg(Surv(time,has.event) ~ 1,data=df,dist="weibull")
  #hack to check shape and rate at working properly
  model$coefficients <- log(1/1.2)
  model$scale <- 2
  model$var <- matrix(c(0.01,-0.01,-0.01,0.005),ncol=2)
  
  x <- FromDataParam(model,type="weibull")
  expect_equal(1.2,x@parameters$rate)
  expect_equal(0.5,x@parameters$shape)
  expect_equal(model$var,x@parameters$sigma)
})


test_that("loglogistic",{
  df <- data.frame(time=rweibull(100,shape=1.1,scale=1.1),has.event=rep(1,100))
  
  model <- survreg(Surv(time,has.event) ~ 1,data=df,dist="loglogistic")
  expect_error(FromDataParam(model,type="weibull"))
  
  x <- FromDataParam(model,type="loglogistic")
  expect_equal("loglogistic",x@type)
  
  rloglogis <- function(n,shape,scale){
    u <- runif(n)
    (u/(1-u))^(1/shape)*scale
  }
  set.seed(10)
  x <- rloglogis(n=1000000,shape=1.2,scale=0.5)
  ans <- survreg(Surv(x,rep(TRUE,length(x)))~1,dist="loglogistic")
  a <- FromDataParam(ans,type="loglogistic")
  
  expect_true(abs(a@parameters$rate-2)<1e-2) #stochastic
  expect_true(abs(a@parameters$shape-1.2)<1e-2)#stochastic 
  
})

