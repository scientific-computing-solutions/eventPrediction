#Predict from parameters
context("followup")

test_that("no_follow_up_and_lag_for_now",{
 
  l1 <- LagEffect(Lag.T=5,L.Ctr.median=3)
  expect_error(SingleArmCRGIStudy(N=100,study.duration=100,ctrl.time=12,ctrl.prop=0.5,k=1,acc.period=10,shape=1,
                             followup=12,lag.settings=l1))
  
  
})


test_that("CRGI_follow_up_non_finite",{
  crgi <- SingleArmCRGIStudy(N=100,study.duration=100,ctrl.time=12,ctrl.prop=0.5,k=1,acc.period=10,shape=1,
                             followup=12)
  
  prediction <- predict(crgi,time.pred=c(22,100),step.size=1)
  expect_equal(c(50,50),prediction@predict.data$events.tot)
  
  crgi <- CRGIStudy(N=100,study.duration=100,ctrl.time=10,ctrl.prop=0.65,k=1,acc.period=10,shape=1.2,
                    alpha=0.05,power=0.6,two.sided=TRUE,HR=0.5,r=1,followup=12)
  
  prediction <- predict(crgi,time.pred=c(22,100),step.size=1)
  rate <- (-log(0.35))^(1/1.2)/10
  prop <- 1-exp(-(12*rate)^1.2)
  ans <- prop*50+50*(1-(1-prop)^0.5) #control + active arm
  
  expect_equal(c(ans,ans),prediction@predict.data$events.tot) #same answer for time=22 and time =100
  
})

test_that("CRGI_inf_followup_isOncology",{
  
  l1 <- LagEffect(Lag.T=5,L.Ctr.median=3,L.HazardRatio=0.4)
  onco <- Study(N=100,study.duration=100,ctrl.median=10,k=1.5,acc.period=10.5,shape=1.2,
                alpha=0.05,power=0.6,two.sided=TRUE,HR=0.5,r=1.3,lag.settings=l1)
  
  
  crgi <- CRGIStudy(N=100,study.duration=100,ctrl.time=10,ctrl.prop=0.5,k=1.5,acc.period=10.5,shape=1.2,
                    alpha=0.05,power=0.6,two.sided=TRUE,HR=0.5,r=1.3,followup=Inf,lag.settings=l1)
  
  
  predict.onco <- predict(onco,step.size=1)
  predict.crgi <- predict(crgi,step.size=1)
  
  expect_equal(predict.onco@grid,predict.crgi@grid)
  
})

context("dropout")

test_that("half_dropout",{
  crgi <- SingleArmCRGIStudy(N=100,study.duration=1000,ctrl.time=12,ctrl.prop=0.5,k=1,acc.period=10,shape=2.1,
                             followup=Inf,dropout=list(time=12,proportion=0.5,shape=2.1))
  
  pred <- predict(crgi,step.size=10,time.pred=1000)
  
  expect_equal(50,pred@predict.data$events.tot[1],tol=1e-7)
  
})

test_that("correct_proportion_dropout",{
  crgi <- SingleArmCRGIStudy(N=100,study.duration=1000,ctrl.time=6,ctrl.prop=0.5,k=1,acc.period=10,shape=2.1,
                             followup=Inf,dropout=list(time=12,proportion=0.5,shape=2.1))
  
  lambda <- log(2)^(1/2.1)/6
  dropout.lambda <- log(2)^(1/2.1)/12
  
  pred <- predict(crgi,step.size=10,time.pred=1000)
  expect_equal(pred@sfns[[1]]@lambda,lambda)
  expect_equal(pred@sfns[[1]]@dropout.lambda,dropout.lambda)
  
  expect_equal(100/(1+(dropout.lambda/lambda)^2.1), pred@predict.data$events.tot[1],tol=1e-7)
  
  
})

test_that("correct_proportion_2_arm",{
  crgi <- CRGIStudy(N=100,study.duration=1000,ctrl.time=6,ctrl.prop=0.5,k=1,acc.period=10,shape=0.8,
                             followup=Inf,dropout=list(time=12,proportion=c(0.5,0.5),shape=0.8),two.sided=TRUE,
                    alpha=0.4,power=0.5,HR=0.75,r=1)
  
  lambda <- log(2)^(1/0.8)/6
  dropout.lambda <- log(2)^(1/0.8)/12
  active.lambda <- log(2)^(1/0.8)/6*0.75^(1/0.8)
  pred <- predict(crgi,step.size=10,time.pred=1000)
  
  expect_equal(pred@sfns[[1]]@lambda,lambda)
  expect_equal(pred@sfns[[1]]@dropout.lambda,dropout.lambda)
  expect_equal(pred@sfns[[2]]@lambda,active.lambda)
  expect_equal(pred@sfns[[2]]@dropout.lambda,dropout.lambda)
  expect_equal(pred@sfns[[2]]@shape,0.8)
  
  expect_equal(50/(1+(dropout.lambda/lambda)^0.8), pred@predict.data$events1[1],tol=1e-7)
  expect_equal(50/(1+(dropout.lambda/active.lambda)^0.8), pred@predict.data$events2[1],tol=1e-7)
  
})

test_that("add_lag",{
  l1 <- LagEffect(Lag.T=5,L.Ctr.median=6)
  crgi <- SingleArmStudy(N=100,study.duration=1000,ctrl.median=12,k=1,acc.period=10,shape=1.2,
                             dropout=list(time=12,proportion=0.85),lag.settings=l1)
  
  
  pred <- predict(crgi,step.size=10,time.pred=c(5,1000))
  
  
  crgi_no_lag <- SingleArmStudy(N=100,study.duration=1000,ctrl.median=6,k=1,acc.period=10,shape=1.2,
                                dropout=list(time=12,proportion=0.85))
  
  pred_no_lag <- predict(crgi_no_lag,step.size=10,time.pred=c(5,1000))
  
  expect_equal(pred@predict.data[1,],pred_no_lag@predict.data[1,])
  expect_false(pred@predict.data[2,"events.tot"]==pred_no_lag@predict.data[2,"events.tot"])
  
  #next try with tiny lag
  l2 <- LagEffect(Lag.T=1e-5,L.Ctr.median=6)
  crgi <- SingleArmStudy(N=100,study.duration=1000,ctrl.median=12,k=1,acc.period=10,shape=1.2,
                         dropout=list(time=12,proportion=0.85),lag.settings=l2)
  pred <- predict(crgi,step.size=10,time.pred=1000)
  pred_no_lag <-  pred <- predict(crgi,step.size=10,time.pred=1000)
  expect_equal(pred@predict.data[1,],pred_no_lag@predict.data[1,])
  
  #and compare with no drop outs
  crgi <- SingleArmStudy(N=100,study.duration=50,ctrl.median=2,k=1,acc.period=1,shape=1.2,lag.settings=l1)
  pred <- predict(crgi,step.size=10,time.pred=50)
  crgi_no_drop <- SingleArmStudy(N=100,study.duration=50,ctrl.median=2,k=1,acc.period=1,shape=1.2,
                         dropout=list(time=12000,proportion=0.0001),lag.settings=l1)
  pred_no_drop <- predict(crgi_no_drop,step.size=10,time.pred=50)
  expect_equal(pred@predict.data[1,]$events1,pred_no_drop@predict.data[1,]$events1,tol=3e-4)
})

test_that("droput_sanity_checks",{
  
  #increase dropout probability decreases number of events
  dropout.prop <- seq(0.05,0.95,0.1)
  
  ans <- unlist(lapply(dropout.prop,
         function(x){
      crgi <- SingleArmCRGIStudy(N=100,study.duration=25,ctrl.time=6,ctrl.prop=0.5,k=1,acc.period=10,shape=2.1,
                             followup=12,dropout=list(time=12,proportion=x,shape=2.1))

         pred <- predict(crgi,step.size=10,time.pred=25)
         return(pred@predict.data[1,]$events1)
         }
  ))
  
  expect_equal(sort(ans,decreasing=TRUE),ans)
  
  #accrual period does not affect dropouts v events within follow up period
  ans2 <- unlist(lapply(dropout.prop,
              function(x){
               crgi <- SingleArmCRGIStudy(N=100,study.duration=25,ctrl.time=6,ctrl.prop=0.5,k=1,acc.period=1,shape=2.1,
                      followup=12,dropout=list(time=12,proportion=x,shape=2.1))
               pred <- predict(crgi,step.size=10,time.pred=25)
              return(pred@predict.data[1,]$events1)
          }
  ))
    
  expect_equal(ans,ans2)
  
  #number of events doesn't decrease
  crgi <- SingleArmCRGIStudy(N=100,study.duration=25,ctrl.time=6,ctrl.prop=0.5,k=1,acc.period=1,shape=2.1,
                             followup=12,dropout=list(time=12,proportion=0.5,shape=12.1))
  pred <- predict(crgi)
  expect_equal(sort(pred@grid$events1),pred@grid$events1)
  
})


test_that("check_sfn",{
  crgi <- SingleArmCRGIStudy(N=100,study.duration=25,ctrl.time=6,ctrl.prop=0.5,k=1,acc.period=10,shape=2.5,
                             followup=12,dropout=list(time=12,proportion=0.5,shape=1.5))
  
  pred <- predict(crgi,step.size=10)
  
  #wolfram alpha:
  #integrate exp(-(x*0.143939150100)^2.5-(x*0.065268314085)^1.5)*2.5*0.143939150100^2.5*x^1.5 dx, 
  #from x=0 to x=5
  expect_equal(1-0.318725,pred@sfns[[1]]@sfn(5),1e-6)
  expect_equal(1-0.625953,pred@sfns[[1]]@sfn(8),1e-6)
  expect_equal(1-0.762519,pred@sfns[[1]]@sfn(20),2e-6) #only as far as 12
  expect_equal(1-0.762519,pred@sfns[[1]]@sfn(200),2e-6)
  
})

test_that("correct_proportion_integral",{
  
  crgi <- SingleArmCRGIStudy(N=100,study.duration=1000,ctrl.time=6,ctrl.prop=0.5,k=1,acc.period=10,shape=1.1,
                             followup=Inf,dropout=list(time=12,proportion=0.5,shape=0.8))
  
  pred <- predict(crgi,step.size=10,time.pred=1000)
  #wolfram alpha integral
  #integrate exp(-(x*0.119438577760)^1.1-(x*0.052704849831)^0.8)*1.1*0.119438577760^1.1*x^0.1 dx,
  #from x=0 to x=1000
  
  expect_equal(100*0.655972,pred@predict.data$events1,tol=1e-6)
})


context("atrisk")

test_that("no_events_or_dropout",{
  crgi <- CRGIStudy(N=100,study.duration=100,ctrl.time=1000,ctrl.prop=0.01,k=1.42,acc.period=10,shape=1,
                    followup=1,two.sided=TRUE,alpha=0.4,power=0.5,HR=0.75,r=1)
  
  #From 11 months onwards everyone should have been on the trial for exactly 1 month 
  #excluding the minor event probabilities  
  pred <- predict(crgi,step.size=10,time.pred=c(11,25,100))
  
  expect_equal(rep(50,3),pred@predict.data$at.risk1,tol=1e-4)
  expect_equal(rep(50,3),pred@predict.data$at.risk2,tol=1e-4)
  expect_equal(pred@predict.data$at.risk1+pred@predict.data$at.risk2,pred@predict.data$atrisk.tot)
})

test_that("no_events_no_followup",{
  crgi <- CRGIStudy(N=100,study.duration=100,ctrl.time=1000,ctrl.prop=0.001,k=1,acc.period=0.01,shape=1,
                    followup=Inf,two.sided=TRUE,alpha=0.4,power=0.5,HR=0.75,r=1)

  pred <- predict(crgi,step.size=10,time.pred=c(10,20,100))
  #Total at risk should be 100*time.pred
  expect_equal(pred@predict.data$atrisk.tot,c(1000,2000,10000),tol=2e-4)
  
})


test_that("all_events",{
  crgi <- SingleArmStudy(N=100,study.duration=1000,ctrl.median=log(2),k=1,acc.period=1,shape=1)
  
  pred <- predict(crgi,step.size=10,time.pred=1000)
  
  #Expected time on treatment E(X) where X is gamma(100,1) = 100
  expect_equal(100,pred@predict.data$atrisk.tot,tol=1e-7)
  
})

test_that("all_dropout_equal_all_event",{
  
  crgi <- SingleArmStudy(N=100,study.duration=1000,ctrl.median=log(2),k=1,acc.period=1,shape=1.5)
  
  pred <- predict(crgi,step.size=10,time.pred=1000)
  
  crgi.dropout <- SingleArmStudy(N=100,study.duration=1000,ctrl.median=10000000,k=1,acc.period=1,shape=1,
                                 dropout=list(time=log(2),proportion=0.5,shape=1.5))
  
  pred.dropout <- predict(crgi.dropout,step.size=10,time.pred=1000)
  
  expect_equal(pred@predict.data$at.risk1,pred.dropout@predict.data$at.risk1,tol=1e-6)
  #at risk should equal 100*mean of the appropriate Weibull distribution
  expect_equal(pred@predict.data$at.risk1,100*gamma(1+1/1.5)/log(2)^(-1/3))
})

