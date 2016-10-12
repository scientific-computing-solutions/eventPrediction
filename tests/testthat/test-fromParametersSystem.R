context("fromParam_systemlevel")

test_that("lag.should.equal.non.lag.in.these.cases",{
  
  l1 <- LagEffect(Lag.T=5,L.Ctr.median=3,L.HazardRatio=0.8)
  l2 <- LagEffect(Lag.T=20,L.Ctr.median=3,L.HazardRatio=0.8)
  
  s1 <- Study(N=1000,study.duration=10,ctrl.median=3,k=1.2,acc.period=5,shape=2,
                       power=0.9,alpha=0.05,r=1,HR=0.8,two.sided=TRUE,
              lag.settings=l1)
  
  s1.nolag <- Study(N=1000,study.duration=10,ctrl.median=3,k=1.2,acc.period=5,shape=2,
              power=0.9,alpha=0.05,r=1,HR=0.8,two.sided=TRUE)
  
  s2 <- Study(N=1000,study.duration=10,ctrl.median=6,k=1.2,acc.period=5,shape=2,
              power=0.9,alpha=0.05,r=1,HR=0.75,two.sided=TRUE,
              lag.settings=l2)
  
  a1 <- predict(s1.nolag,time.pred=c(5,8,10),event.pred=c(10,15))
  
  
  
  a2 <- predict(s1,time.pred=c(5,8,10),event.pred=c(10,15))
  a3 <- predict(s2,time.pred=c(5,8,10),event.pred=c(10,15))
  
  #remove atrisk columns for check:
  p1 <- a1@predict.data
  p2 <- a2@predict.data
  
  p1$at.risk1 <- NULL
  p1$at.risk2 <- NULL
  p1$atrisk.tot <- NULL
  p2$at.risk1 <- NULL
  p2$at.risk2 <- NULL
  p2$atrisk.tot <- NULL
  
  expect_equal(p1,p2) 
  
  #Then check atrisk columns
  expect_equal(a1@predict.data$at.risk1,a2@predict.data$at.risk1,tol=1e-7)
  expect_equal(a1@predict.data$at.risk2,a2@predict.data$at.risk2,tol=1e-7)
  expect_equal(a1@predict.data$atrisk.tot,a2@predict.data$atrisk.tot,tol=1e-7)
  
  expect_equal(a1@grid,a2@grid)
  
  expect_equal(a1@av.hr,a2@av.hr)
  expect_equal(a1@critical.events.req,a2@critical.events.req)
  
  
  
  expect_equal(a1@critical.data$at.risk1,a2@critical.data$at.risk1,tol=1e-7)
  expect_equal(a1@critical.data$at.risk2,a2@critical.data$at.risk2,tol=1e-7)
  expect_equal(a1@critical.data$atrisk.tot,a2@critical.data$atrisk.tot,tol=1e-7)
  
  
  
  expect_equal(a1@sfns[[1]]@lambda,a2@sfns[[1]]@lambda)
  expect_equal(a1@sfns[[2]]@lambda,a2@sfns[[2]]@lambda)
  expect_equal(a2@sfns[[1]]@lambda,a2@sfns[[1]]@lambdaot)
  expect_equal(a2@sfns[[2]]@lambda,a2@sfns[[2]]@lambdaot)
  
  #remove atrisk columns for check:
  p1 <- a1@predict.data
  p2 <- a3@predict.data
  
  p1$at.risk1 <- NULL
  p1$at.risk2 <- NULL
  p1$atrisk.tot <- NULL
  p2$at.risk1 <- NULL
  p2$at.risk2 <- NULL
  p2$atrisk.tot <- NULL
  
  expect_equal(p1,p2) 
  
  #Then check atrisk columns
  expect_equal(a1@predict.data$at.risk1,a3@predict.data$at.risk1,tol=1e-7)
  expect_equal(a1@predict.data$at.risk2,a3@predict.data$at.risk2,tol=1e-7)
  expect_equal(a1@predict.data$atrisk.tot,a3@predict.data$atrisk.tot,tol=1e-7)
  
  expect_equal(a1@grid,a3@grid)
  expect_equal(a1@av.hr,a3@av.hr)
  expect_equal(a1@critical.events.req,a3@critical.events.req)
  expect_equal(a1@critical.data,a3@critical.data)
  
  expect_equal(a1@sfns[[1]]@lambda,a3@sfns[[1]]@lambdaot)
  expect_equal(a1@sfns[[2]]@lambda,a3@sfns[[2]]@lambdaot)
})

test_that("event.and.time.pred.have.symmetry",{
  #Note due to rounding of events the 'symmetry' only works one way...
  
  s1 <- Study(N=1000,study.duration=10,ctrl.median=3,k=1.2,acc.period=5,shape=2,
              power=0.9,alpha=0.05,r=1,HR=0.8,two.sided=TRUE)
  
  a1 <- predict(s1,event.pred=c(100,200,300,400))
  
  time.pred <- a1@predict.data[,"time"]
  
  a2 <- predict(s1,time.pred=time.pred)
  
  expect_true(all(a2@predict.data[,"time.pred"]))
  expect_false(any(a1@predict.data[,"time.pred"]))
  
  a1@predict.data$time.pred <- rep(TRUE,4)
  
  expect_equal(a1@predict.data,a2@predict.data)  
  
})


test_that("recruit",{
  
  k <- c(0.5,1,2)
  
  lapply(k,function(x){
    s <- SingleArmStudy(N=1000,study.duration=10,ctrl.median=3,k=x,acc.period=5,shape=1.2)
    a <- predict(s,time.pred=5/(2^(1/x)))
    expect_equal(1/(2^x)*1000,a@grid[a@grid$time==2.5,]$recruit.tot)
    expect_equal(500,a@predict.data[1,]$recruit.tot)
  })
  
  lapply(k,function(x){
    s <- Study(N=1000,study.duration=10,ctrl.median=3,k=x,acc.period=6,
               shape=1.2,power=0.8,alpha=0.05,two.sided=TRUE,r=1.2,HR=0.8)
    a <- predict(s,time.pred=6/(2^(1/x)))
    expect_equal(1/(2^x)*1000,a@grid[a@grid$time==3,]$recruit.tot)
    expect_equal(500,a@predict.data[1,]$recruit.tot)
  })
  
})


#this fails on versions < 1.0
test_that("max.num.events",{
  l <- LagEffect(Lag.T=5,L.Ctr.median=1,L.HazardRatio=0.5)
  
  s <- Study(N=800,study.duration=36,ctrl.median=3,k=1,acc.period=20,
             shape=1,power=0.8,alpha=0.05,two.sided=TRUE,r=1.2,HR=0.5,lag.settings=l)
  
  a <- predict(s)
  
  expect_true(all(a@grid$events.tot <= 800))
  
  
})

test_that("median",{
  
  ctrl.median <- c(1:5)
  lapply(ctrl.median,function(x){
    s1 <- SingleArmStudy(N=2000,study.duration=20,ctrl.median=x,k=0.01,acc.period=0.000001,shape=1.5)
    a <- predict(s1,event.pred=1000)
    expect_equal(x,a@predict.data[1,]$time)
    
    })
})

test_that("single.arm.matches.2.arm",{
   
  s1 <- Study(N=2000,study.duration=10,ctrl.median=3,k=1.2,acc.period=5,shape=2,
              power=0.9,alpha=0.05,r=1,HR=0.8,two.sided=TRUE)
  
  s2 <- SingleArmStudy(N=1000,study.duration=10,ctrl.median=3,k=1.2,acc.period=5,shape=2)
  
  s3 <- Study(N=2000,study.duration=10,ctrl.median=3*sqrt(0.8),k=1.2,acc.period=5,shape=2,
              power=0.9,alpha=0.05,r=1,HR=0.8,two.sided=TRUE)
  
  a1 <- predict(s1)
  a2 <- predict(s2)
  a3 <- predict(s3)
  
  expect_equal(a1@grid$events1,a2@grid$events.tot)
  expect_equal(a3@grid$events2,a2@grid$events.tot)
})


str1 <- "800 patients recruited, ratio nE/nC=1, 20 months accrual (non-uniform\naccrual, k=2). Lag time: T=4 months, Control for [0,T] median=3 months and\nfor [T,S] Control median=2 months. Exponential survival function.\nHR([0,T])=1 and HR([T,S])=0.5, which gives an average HR=0.77. For a study\nwith no lag and this HR: critical HR value=0.83, alpha(2-sided)=5%,\npower=80%, 466 events required and using the given lag settings: expected\nat time 18.9 months (Experimental/Control: 225/241)."
str2 <- "120 patients recruited, ratio nE/nC=0.5, 20 months accrual (uniform\naccrual, k=1). Control median=2.5 months (lambda=0.29). Experimental\nmedian=4.11 months (lambda=0.18). Weibull survival function shape=1.2.\nHR(Experimental:Control)=0.55, critical HR value=0.71, alpha(1-sided)=5%,\npower=90%, 108 events required expected at time 22.6 months\n(Experimental/Control: 33/75). At (10, 3.2) months the predicted number of\nevents is (37, 5) [Experimental/Control: (10, 1)/(27, 4)]."
str3 <- "100 patients recruited, 5 months accrual (non-uniform accrual, k=1.5). Lag\ntime: T=4 months, Control for [0,T] median=2.5 months and for [T,S]\nControl median=3 months. Weibull survival function shape=2.  At 10 months\nthe predicted number of events is 97."

summary_check <- function(study,time.pred,event.pred,stringoutput){
  prediction <- predict(study,time.pred=time.pred, event.pred=event.pred)
  x <- eventPrediction:::getFromParameterText(prediction,DisplayOptions(text.width=75))
  expect_equal(x,stringoutput)
}

test_that("summary_output",{
  
  lagged <- LagEffect(Lag.T = 4,L.Ctr.median = 3,  
                      L.HazardRatio=1 )
  
  study <- Study(alpha = 0.05,power = 0.8,
    HR = 0.5, r = 1,N = 800,
    study.duration = 30,ctrl.median = 2,
    k = 2,acc.period = 20,two.sided = TRUE,lag.settings=lagged)
  
  
  summary_check(study,NULL,NULL,str1) 
  
  study <- Study(alpha = 0.05,power = 0.9,
                 HR = 0.55, r = 0.5,N = 120,
                 study.duration = 25,ctrl.median = 2.5,
                 k = 1,acc.period = 20,two.sided = FALSE,shape=1.2)
  
  summary_check(study,10,5,str2)
  lagged <- LagEffect(Lag.T = 4,L.Ctr.median = 2.5)
  study <- SingleArmStudy(N=100,study.duration=10,ctrl.median=3,k=1.5,
                          acc.period=5,shape=2,lag.settings=lagged)
 
  summary_check(study,10,NULL,str3)
})


test_that("critical_hr",{
  # this compares to the output from non-proportional hazards package
  #there are slight differences in the rounding 
  lagged <- LagEffect(Lag.T = 4,L.Ctr.median = 11,  
                      L.HazardRatio=1 )
  
  study <- Study(alpha = 0.05,power = 0.8036576,
                 HR = 0.6, r =1,N = 780,
                 study.duration = 25.5,ctrl.median = 11,
                 k = 2,acc.period = 18,two.sided = TRUE,shape=1,lag.settings=lagged)
  
  
  prediction <- predict(study)
  
  nph.answer <- 0.8198656
  
  expect_equal(nph.answer,prediction@critical.HR,tol=5e-3)
  
  lagged <- LagEffect(Lag.T = 6,L.Ctr.median = 8,  
                      L.HazardRatio=1 )
  
  study <- Study(alpha = 0.05,power = 0.9829889,
                 HR = 0.5, r =1.5,N = 800,
                 study.duration = 30,ctrl.median = 8,
                 k = 0.5,acc.period = 18,two.sided = TRUE,shape=1,lag.settings=lagged)
 
  
  prediction <- predict(study)
  
  
  nph.answer <- 0.8534732
  expect_equal(nph.answer,prediction@critical.HR,tol=5e-3)
  
  lagged <- LagEffect(Lag.T = 5,L.Ctr.median = 8,  
                      L.HazardRatio=1 )
  
  study <- Study(alpha = 0.05,power = 0.9989243,
                 HR = 0.5, r =1.5,N = 800,
                 study.duration = 30,ctrl.median = 8,
                 k = 0.5,acc.period = 6,two.sided = TRUE,shape=1,lag.settings=lagged)
  
  
  prediction <- predict(study)
  
  
  nph.answer <- 0.8572458
  expect_equal(nph.answer,prediction@critical.HR,tol=5e-3)
  
})

test_that("average hazard ratio",{
  # this compares to the output from non-proportional hazards package
  #results do not match exactly: nph uses interval bisection to some tolerance
  #whereas eventPrediction calculates the exact integral and is accruate up to 
  #R's integrate functions accruacy  
  #Also there are slight differences in the rounding of w1 and w2 (see eventPrediction vignette)
  #which will cause small differences to the average HR
  
  lagged <- LagEffect(Lag.T = 4,L.Ctr.median = 11,  
                      L.HazardRatio=1 )
  
  study <- Study(alpha = 0.05,power = 0.9,
                 HR = 0.6, r =1,N = 780,
                 study.duration = 25.5,ctrl.median = 11,
                 k = 2,acc.period = 18,two.sided = FALSE,shape=1,lag.settings=lagged)
 
  
  prediction <- predict(study)
  
  nph.answer <- 0.7518394
  expect_equal(nph.answer,prediction@av.hr,tol=1.1e-3)
  
  lagged <- LagEffect(Lag.T = 6,L.Ctr.median = 8,  
                      L.HazardRatio=1 )
  
  study <- Study(alpha = 0.05,power = 0.9,
                 HR = 0.5, r =1.5,N = 800,
                 study.duration = 30,ctrl.median = 8,
                 k = 0.5,acc.period = 18,two.sided = FALSE,shape=1,lag.settings=lagged)
  
  
  prediction <- predict(study)
  
  
  nph.answer <- 0.7190647
  expect_equal(nph.answer,prediction@av.hr,tol=1.1e-3)
  
  lagged <- LagEffect(Lag.T = 5,L.Ctr.median = 8,  
                      L.HazardRatio=1 )
  study <- Study(alpha = 0.05,power = 0.5,
                 HR = 0.75, r =1.5,N = 800,
                 study.duration = 30,ctrl.median = 8,
                 k = 0.5,acc.period = 6,two.sided = FALSE,shape=1,lag.settings=lagged)
  
  
  prediction <- predict(study)
  
   
  nph.answer <- 0.8417572
  expect_equal(nph.answer,prediction@av.hr,tol=1.1e-3)
  
})


