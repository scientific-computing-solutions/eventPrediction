context("fromParametersInternal")

test_that("getNS",{
  expect_equal(c(10,0),getNs(TRUE,r=1,N=10))
  expect_equal(c(6,0),getNs(TRUE,r=0,N=6))
  
  expect_equal(c(50,50),getNs(FALSE,r=1,N=100))
  expect_equal(c(25,75),getNs(FALSE,r=3,N=100))
  expect_equal(c(150,50),getNs(FALSE,r=1/3,N=200))
  
  expect_equal(c(44,56),getNs(FALSE,r=1.3,N=100))
  
  expect_equal(c(134,266),getNs(FALSE,r=2,N=400))
  expect_equal(c(267,133),getNs(FALSE,r=1/2,N=400))
  
  
  expect_error(getNS(FALSE,r=1/9, N=6))
  
})

test_that("events_required",{
  
  expect_true(abs(RequiredEvents(1,0.05,0.9,0.9,3086)-3085.838 ) < 1e-3)
  expect_warning(RequiredEvents(1,0.05,0.9,0.9,1000))
  expect_warning(RequiredEvents(1,0.05,0.9,0.9,3085))

  
  expect_true(abs(RequiredEvents(3,0.05,0.9,0.4,10000)-54.40027 ) < 1e-3)
  
  expect_equal(RequiredEvents(2,0.1,0.95,0.9,10000),RequiredEvents(2,0.05,0.9,0.9,10000))
  
  expect_true(abs(RequiredEvents(4,pnorm(0.1),pnorm(0.9),exp(-2/5),100)-25.0) < 1e-3)
  
})

test_that("lambda.calc",{
  
  l1 <- lambda.calc(1,NA,1)
  expect_true(is.na(l1[2]))
  expect_equal(log(2),l1[1])
  
  expect_error(lambda.calc(c(1,2),NA,1))
  expect_error(lambda.calc(1,c(1,2),1))
  
  expect_equal( c(log(2)^0.5/4,0.8^0.5*log(2)^0.5/4 ),lambda.calc(4,0.8,2) )
})

          
test_that("event.integrate",{
  
  #events.integ <- function( SurvFn, B, k, t) 
  s <- GetSurvivalFunctions(2,NA,lag.T=0,isSingleArm=TRUE,shape=1,followup=Inf,dropout.shape=1,dropout.lambda=0)
  
  expect_equal(0,events.integ(s[[2]],1,1,2))
  
  #first k=1 with B=1
  
  #have m/B - [exp(m*lambda)-1]*[exp(-lambda*t)]/[B*lambda]
  ans <- 1-exp(-2*2)*(exp(2)-1)/2
  expect_equal(ans,events.integ(s[[1]],B=1,k=1,t=2))
  ans2 <-0.5-exp(-1)*(exp(1)-1)/2
  expect_equal(c(ans,ans2),events.integ(s[[1]],B=1,k=1,t=c(2,0.5)))
  
  #next keep k=1 and set B=2.5
  ans <- 2/2.5-exp(-2*2)*(exp(2*2)-1)/(2*2.5)
  ans2 <-1-exp(-2*3)*(exp(2.5*2)-1)/(2*2.5)
  
  expect_equal(c(ans,ans2),events.integ(s[[1]],B=2.5,k=1,t=c(2,3)))
  
  #Next have a go with k=3, shape=1
  k_equal_three <- function(m,B,lambda,t){
    (m/B)^3 - 3*exp(-lambda*t)*(exp(lambda*m)*(lambda*m*(lambda*m-2)+2)-2)/(B^3*lambda^3)
  }
  
  ans <- k_equal_three(1,4,2,1)
  ans2 <- k_equal_three(4,4,2,5)
  expect_equal(c(ans,ans2),events.integ(s[[1]],B=4,k=3,t=c(1,5)))
  
  #Try k=2, shape =2 just because we can
  erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
  integral_val_yes_really <- function(m,B,l,t){
    (m/B)^2-(sqrt(pi)*l*t*(erf(l*(m-t))+erf(l*t))-exp(-l^2*(m-t)^2)+exp(-l^2*t^2) )/(B^2*l^2)
  }
  
  
  s <- GetSurvivalFunctions(2,NA,lag.T=0,isSingleArm=TRUE,shape=2,followup=Inf,dropout.shape=1,dropout.lambda=0)
  ans <-  integral_val_yes_really(1,4,2,1)
  ans2 <-  integral_val_yes_really(4,4,2,5)
  
  expect_equal(c(ans,ans2),events.integ(s[[1]],B=4,k=2,t=c(1,5)))
  
  #Simple case k=1 with a lag
  s <- GetSurvivalFunctions(2,4,lag.T=1,isSingleArm=TRUE,shape=2,followup=Inf,dropout.shape=1,dropout.lambda=0)
  
  expect_equal(c(0,1),events.integ(s[[1]],B=4,k=2,t=c(0,Inf)))
  
  
  
})          
          
          
test_that("surivalfunctions_with_followup",{
  isSingleArm <- TRUE
  lag.T <- 0
  shape <- 1.2
  
  s <- GetSurvivalFunctions(0.02,NA,lag.T,isSingleArm,shape,followup=12,dropout.shape=1,dropout.lambda=0)
  
  expect_equal(s[[1]]@sfn(c(12,14,100,Inf)),rep(exp(-(12*0.02)^shape),4))
  expect_false(s[[1]]@sfn(11.9)==exp(-(12*0.02)^shape))  
  s.n.follow <- GetSurvivalFunctions(0.02,NA,lag.T,isSingleArm,shape,followup=Inf,dropout.shape=1,dropout.lambda=0)
  expect_equal(s.n.follow[[1]]@sfn(c(1,4,12)),s[[1]]@sfn(c(1,4,12)))
  
  
})          
          
          
          
test_that("get_survival_functions_no_lag",{

  isSingleArm <- TRUE
  lag.T <- 0
  shape <- 1.2
  
  s <- GetSurvivalFunctions(4,NA,lag.T,isSingleArm,shape,followup=Inf,dropout.shape=1,dropout.lambda=c(0,0))
  
  expect_true(is.vector(s))
  expect_equal(2,length(s))
   
  expect_equal(s[[2]],NullSfn())
  
  e1 <- as.list(environment(s[[1]]@sfn))
  expect_equal(4,e1$lambda)
  expect_equal(shape,e1$shape)
  
  x <- seq(0,20,0.1)
  y1 <- s[[1]]@sfn(x)
  y2 <- exp(-(4*x)^shape)
  
  expect_equal(y2,y1)
  
  shape <- 1
  isSingleArm <- FALSE
  s2 <- GetSurvivalFunctions(c(4,2),NA,lag.T,isSingleArm,shape,followup=Inf,dropout.shape=1,dropout.lambda=c(0,0))
  
  expect_true(is.vector(s))
  expect_equal(2,length(s))

  
  y1 <- s2[[1]]@sfn(x) 
  y2 <- exp(-(4*x)^shape)
  expect_equal(y1,y2)
  
  y1 <- s2[[2]]@sfn(x) 
  y2 <- exp(-(2*x)^shape)
  expect_equal(y1,y2)
  
  
})


test_that("get_survival_functions_with_lag",{
  
  isSingleArm <- TRUE
  lag.T <- 5
  shape <- 1.2
  
  s <- GetSurvivalFunctions(4,3,lag.T,isSingleArm,shape,followup=Inf,dropout.shape=1,dropout.lambda=c(0,0))
  
  expect_true(is.vector(s))
  expect_equal(2,length(s))
  expect_equal(s[[2]],NullSfn())
  
  e1 <- as.list(environment(s[[1]]@sfn))
  expect_equal(4,e1$lambda)
  expect_equal(shape,e1$shape)
  expect_equal(lag.T,e1$lag.T)
  expect_equal(3,e1$lambdaot)
  
  x <- seq(0,20,0.1)
  y1 <- s[[1]]@sfn(x)
  y2 <- ifelse(x<lag.T, exp(-(3*x)^shape),exp(-(4*x)^shape+(3^shape-4^shape)*lag.T^shape)) 
    
  
  expect_equal(y2,y1)
  
  lag.T <- 3
  isSingleArm <- FALSE
  shape <- 1.32
  s <- GetSurvivalFunctions(c(4,2),c(3,1),lag.T,isSingleArm,shape,followup=Inf,dropout.shape=1,dropout.lambda=c(0,0))
  y1 <- s[[1]]@sfn(x)
  y2 <- ifelse(x<lag.T, exp(-(3*x)^shape),exp(-(4*x)^shape+(4^shape-3^shape)*lag.T^shape)) 
  expect_equal(y1,y2)
  
  y1 <- s[[2]]@sfn(x)
  y2 <- ifelse(x<lag.T, exp(-(x)^shape),exp(-(2*x)^shape+(2^shape-1)*lag.T^shape)) 
  expect_equal(y1,y2)
  
})


