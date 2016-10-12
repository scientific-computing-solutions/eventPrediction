context("test-ctrlSpec")

test_that("give_median",{
  expect_error(CtrlSpecfromMedian(ctrl.median=NA)) 
  expect_error(CtrlSpecfromMedian(ctrl.median=-6))
  expect_error(CtrlSpecfromMedian(ctrl.median="ew"))
  expect_warning(expect_error(CtrlSpecfromMedian(ctrl.median=c(1,2,3))))
  
  cs <- CtrlSpecfromMedian(ctrl.median=10)
  expect_equal(10,cs@median)
  expect_equal("median=10",cs@text)
  
  expmed <- cs@experimentaltext(HR=1,shape=1)
  expect_equal("median=10",expmed)
  
  expmed <- cs@experimentaltext(HR=1,shape=1)
  expect_equal("median=10",expmed)
  
  expmed <- cs@experimentaltext(HR=1/1.5672,shape=1)
  expect_equal("median=15.67",expmed)
  
  expmed <- cs@experimentaltext(HR=0.25,shape=0.5)
  expect_equal("median=160",expmed)
  
})

test_that("give_prop",{
  expect_error(CtrlSpecfromProportion(time=-8,proportion.had.event=0.5,shape=1))
  expect_error(CtrlSpecfromProportion(time=8,proportion.had.event=-0.5,shape=1))
  expect_error(CtrlSpecfromProportion(time=8,proportion.had.event=1.0,shape=1))
  expect_error(CtrlSpecfromProportion(time=8,proportion.had.event=0.5,shape=0))
  expect_error(CtrlSpecfromProportion(time=8,proportion.had.event=0.5,shape=c(1,4)))
  
  x <- CtrlSpecfromProportion(time=8,proportion.had.event=0.75,shape=2.5)
  m <- (log(2)^0.4)/(log(4)^0.4/8)
  expect_equal(m,x@median)
  
  expect_equal("75% of subjects had an event by 8",x@text)
  
  x <- CtrlSpecfromProportion(time=8.5,proportion.had.event=0.5,shape=2)
  m <- 8.5
  expect_equal(m,x@median)
  expect_equal("50% of subjects had an event by 8.5",x@text)
  
  
  expect_equal("50% of subjects had an event by 8.5",x@experimentaltext(HR=1,shape=2))
  
  prop <- paste(round((1-(1-0.5)^2)*100,2),"% of subjects had an event by 8.5",sep="")
  expect_equal(prop, x@experimentaltext(HR=2,shape=2))
  
})
