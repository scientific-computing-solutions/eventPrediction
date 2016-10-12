#Test the common.R functions note getNS is tested elsewhere

context("common")

test_that("RootFindRateShape",{
  mu <- sqrt(pi)          
  sigma <- 2*sqrt(1-pi/4)
  
  ans <- RootFindRateShape(mu,sigma)
  expect_equal(2,ans[["shape"]],tol=1e-5)      
  expect_equal(0.5,ans[["rate"]],tol=1e-5)
})

test_that("roundForceOutputZeros",{
  expect_equal("23.5",roundForceOutputZeros(23.48,dp=1))
  expect_equal("-23.545",roundForceOutputZeros(-23.545,dp=3))
  expect_equal("23.50",roundForceOutputZeros(23.5,dp=2))
  expect_equal("23.500",roundForceOutputZeros(23.5,dp=3))
  expect_equal("23.5000000000",roundForceOutputZeros(23.5,dp=10))
  expect_error(roundForceOutputZeros(0.765,dp=0))
})

test_that("FitMixtureModel",{
  
  expect_error(FitMixtureModel(HR=-1,r=1,M=1))
  expect_error(FitMixtureModel(HR=1,r=0,M=1))
  expect_error(FitMixtureModel(HR=1,r=1,M=0))
  expect_error(FitMixtureModel(HR=1,r=1,M=c(1,2,3)))
  expect_error(FitMixtureModel(HR="de",r=1,M=0))
  
  x <-FitMixtureModel(HR=1,r=1,M=5)
  expect_equal(1,x$shape,tol=2e-6) #tolerance as numerics used for fit
  expect_equal(log(2)/5,x$rate,tol=2e-6)
  
})