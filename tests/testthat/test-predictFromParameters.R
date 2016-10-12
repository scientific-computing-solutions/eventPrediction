context("predictFromParameters")

test_that("test_lag",{
  l1 <- LagEffect(Lag.T=3,L.Ctr.median=6.5,L.HazardRatio=0.7)
  
  expect_equal(3,l1@Lag.T)
  expect_equal(6.5,l1@ctrlSpec@median)
  expect_equal(0.7,l1@L.HazardRatio)
  
  expect_error(LagEffect(Lag.T=-5,L.Ctr.median=6.5,L.HazardRatio=0.7))
  expect_error(LagEffect(Lag.T=5,L.Ctr.median=-4,L.HazardRatio=0.7))
  expect_error(LagEffect(Lag.T=5,L.Ctr.median=4,L.HazardRatio=1.3))
  expect_error(LagEffect(Lag.T=5,L.Ctr.median=4,L.HazardRatio=-0.4))
  expect_error(LagEffect(Lag.T=5,L.Ctr.median=4,L.HazardRatio="sedf"))
  expect_warning(LagEffect(Lag.T=c(1,5),L.Ctr.median=4,L.HazardRatio=0.4))
  
  l4 <- LagEffect(Lag.T=5.2,L.Ctr.median=4)
  expect_true(is.na(l4@L.HazardRatio))
  expect_error(LagEffect(Lag.T=5,L.Ctr.median=as.numeric(NA),L.HazardRatio=as.numeric(NA)))
  
  l2 <- NullLag()
  l3 <- LagEffect(Lag.T=0)

  expect_true(isNullLag(l2))
  expect_true(isNullLag(l3))
  expect_false(isNullLag(l1))
  
  expect_output(print(l2),"No Lag")
  expect_output(print(l4),"5.2 months of lag during which\ncontrol group survival median=4 months")
  expect_output(print(l1),"3 months of lag during which\ncontrol group survival median=6.5 months\nand the hazard ratio is 0.7 ")
})



test_that("valid_single_arm_study",{
  s1 <- SingleArmStudy(N=100,study.duration=10,ctrl.median=3,k=1.5,acc.period=5,shape=2)
  
  expect_equal(NullLag(),s1@lag.settings)
  expect_equal(100,s1@N)
  expect_equal(10,s1@study.duration)
  expect_equal(3,s1@ctrlSpec@median)
  expect_equal(1.5,s1@k)
  expect_equal(5,s1@acc.period)
  expect_equal(2,s1@shape)
  
  expect_true(is.na(s1@HR))
  expect_true(is.na(s1@alpha))
  expect_true(is.na(s1@power))
    
  expect_true(isSingleArm(s1))
  
  
  l1 <- LagEffect(Lag.T=5,L.Ctr.median=3)
  s2 <- SingleArmStudy(N=100,study.duration=20,ctrl.median=10.2,k=0.5,acc.period=10.5,lag.settings=l1)
  expect_equal(1,s2@shape)
  expect_equal(5,s2@lag.settings@Lag.T)
})


test_that("invalid_single_arm_study",{
  expect_error(SingleArmStudy(N=0,study.duration=100,ctrl.median=3,k=1.5,acc.period=10.5,shape=2))
  expect_error(SingleArmStudy(N="as",study.duration=100,ctrl.median=3,k=1.5,acc.period=10.5,shape=2))
  expect_error(SingleArmStudy(N=c(3,4),study.duration=100,ctrl.median=3,k=1.5,acc.period=10.5,shape=2))
  expect_error(SingleArmStudy(N=100,study.duration=0,ctrl.median=3,k=1.5,acc.period=10.5,shape=2))
  expect_error(SingleArmStudy(N=100,study.duration=-1,ctrl.median=3,k=1.5,acc.period=10.5,shape=2))
  expect_error(SingleArmStudy(N=100,study.duration=100,ctrl.median=-1,k=1.5,acc.period=10.5,shape=2))
  expect_error(SingleArmStudy(N=100,study.duration=100,ctrl.median=10,k=0,acc.period=10.5,shape=2))
  expect_error(SingleArmStudy(N=100,study.duration=100,ctrl.median=10,k=-2,acc.period=10.5,shape=2))
  expect_error(SingleArmStudy(N=100,study.duration=100,ctrl.median=10,k=2,acc.period=10.5,shape=0))
  expect_error(SingleArmStudy(N=100,study.duration=100,ctrl.median=10,k=2,acc.period=-10.5,shape=2))
  expect_error(SingleArmStudy(N=100,study.duration=100,ctrl.median=10,k=2,acc.period=0,shape=2))
  expect_error(SingleArmStudy(N=100,study.duration=2.5,ctrl.median=10,k=2,acc.period=5))
  expect_error(SingleArmStudy(N=100.5,study.duration=100,ctrl.median=3,k=1.5,acc.period=10.5,shape=2))
  
  expect_error(SingleArmStudy(N=100,study.duration=20,ctrl.median=10.2,k=0.5,acc.period=10.5,lag.settings=4))
  
  l1 <- LagEffect(Lag.T=5,L.Ctr.median=3,L.HazardRatio=0.8)
  #cannot have HR in single arm study
  expect_error(SingleArmStudy(N=100,study.duration=20,ctrl.median=10.2,k=0.5,acc.period=10.5,lag.settings=l1))
  
  
})


test_that("valid_two_arm",{
  #Note only the extra arguments not used in SingleArm study are tested here 
  
  s1 <- Study(N=100,study.duration=100,ctrl.median=3,k=1.5,acc.period=10.5,shape=1.2,
              alpha=0.05,power=0.6,two.sided=TRUE,HR=0.75,r=1.3)
  
  expect_equal(0.05,s1@alpha)
  expect_equal(0.6,s1@power)
  expect_equal(0.75,s1@HR)
  expect_equal(1.3,s1@r)
  expect_equal(1.2,s1@shape)
  expect_true(s1@two.sided)
  
  
  s2 <- Study(N=100,study.duration=100,ctrl.median=3,k=1.5,acc.period=10.5,
              alpha=0.95,power=0.1,two.sided=FALSE,HR=0.9,r=0.85)
  
  expect_false(s2@two.sided)
  expect_equal(1,s2@shape)
  
  expect_equal(s2@dropout.shape,1)
  expect_true(is.infinite(s2@dropout[[1]]@median))
  expect_true(is.infinite(s2@dropout[[2]]@median))
  
})

test_that("invalid_two_arm",{
  #Note only the extra arguments not used in SingleArm study are tested here  
  
  #cannot have lag without HR in 2 arm study
  l1 <- LagEffect(Lag.T=5,L.Ctr.median=3)
  expect_error(Study(N=100,study.duration=100,ctrl.median=3,k=1.5,acc.period=10.5,
                     alpha=0.95,power=0.1,two.sided=FALSE,HR=0.9,r=0.85,lag.settings=l1))
  
  
  expect_error(Study(N=100,study.duration=100,ctrl.median=3,k=1.5,acc.period=10.5,shape=1.2,
              alpha=1.2,power=0.6,two.sided=TRUE,HR=0.75,r=1.3))
  
  expect_error(Study(N=100,study.duration=100,ctrl.median=3,k=1.5,acc.period=10.5,shape=1.2,
              alpha=0,power=0.6,two.sided=TRUE,HR=0.75,r=1.3))
  
  expect_error(Study(N=100,study.duration=100,ctrl.median=3,k=1.5,acc.period=10.5,shape=1.2,
              alpha=0.05,power=-1,two.sided=TRUE,HR=0.75,r=1.3))
  
  expect_error(Study(N=100,study.duration=100,ctrl.median=3,k=1.5,acc.period=10.5,shape=1.2,
              alpha=0.05,power=1.0,two.sided=TRUE,HR=0.75,r=1.3))
  
  expect_error(Study(N=100,study.duration=100,ctrl.median=3,k=1.5,acc.period=10.5,shape=1.2,
              alpha=0.05,power=0.6,two.sided=TRUE,HR=1,r=1.3))
  
  expect_error(Study(N=100,study.duration=100,ctrl.median=3,k=1.5,acc.period=10.5,shape=1.2,
              alpha=0.05,power=0.6,two.sided=TRUE,HR=0,r=1.3))
  
  expect_error(Study(N=100,study.duration=100,ctrl.median=3,k=1.5,acc.period=10.5,shape=1.2,
              alpha=0.05,power=0.6,two.sided=TRUE,HR=0.75,r=0))
  
  expect_error(Study(N=100,study.duration=100,ctrl.median=3,k=1.5,acc.period=10.5,shape=1.2,
              alpha=0.05,power=0.6,two.sided=TRUE,HR=-0.75,r=1.3))
})


test_that("invalid_arguments_to_predict",{
  
  s <- Study(N=100,study.duration=100,ctrl.median=3,k=1.5,acc.period=10.5,
              alpha=0.95,power=0.1,two.sided=FALSE,HR=0.9,r=0.85)
  
  expect_error(predict(s,time.pred=NA))
  expect_error(predict(s,time.pred="awe"))
  expect_error(predict(s,time.pred=c(-3,5)))
  expect_warning(predict(s,time.pred=c(12,15))) #critical number of events too high
  expect_error(predict(s,event.pred="ef"))
  expect_error(predict(s,event.pred=-4))  
  expect_error(predict(s,event.pred=c(NA,10)))
  expect_warning(predict(s,event.pred=101)) 
  expect_error(predict(s,event.pred=12.5))
  
  expect_error(predict(s,step.size=0))
  expect_error(predict(s,step.size=-2))
  expect_warning(predict(s,step.size=11)) #critical number of events too high
  expect_error(predict(s,step.size=c(0.1,3)))
  expect_error(predict(s,step.size="gre"))
})


test_that("CRGI_study",{
  #focusing testing on arguments which differ from Study constructor
  #see tests on ctrlSpec for validity tests on  ctrl.median type arguments
  
  #give ctrl.median
  expect_error( CRGIStudy(N=100,study.duration=100,ctrl.median=3,k=1.5,acc.period=10.5,shape=1.2,
                      alpha=0.05,power=0.6,two.sided=TRUE,HR=0.75,r=1.3))
  #no follow up
  expect_error( CRGIStudy(N=100,study.duration=100,ctrl.time=8,ctrl.proportion=0.5,k=1.5,acc.period=10.5,shape=1.2,
                          alpha=0.05,power=0.6,two.sided=TRUE,HR=0.75,r=1.3))
  
  #follow up negative
  expect_error( CRGIStudy(N=100,study.duration=100,ctrl.time=8,ctrl.proportion=0.5,k=1.5,acc.period=10.5,shape=1.2,
                          alpha=0.05,power=0.6,two.sided=TRUE,HR=0.75,r=1.3,followup=-5))
  
  expect_error( CRGIStudy(N=100,study.duration=100,ctrl.time=8,ctrl.proportion=0.5,k=1.5,acc.period=10.5,shape=1.2,
                          alpha=0.05,power=0.6,two.sided=TRUE,HR=0.75,r=1.3,followup=c(4,5,6)))
  

  expect_error(SingleArmCRGIStudy(N=100,study.duration=100,ctrl.median=3,k=1.5,acc.period=10.5,shape=2)  )
  expect_error(SingleArmCRGIStudy(N=100,study.duration=100,ctrl.time=8,ctrl.proportion=0.5,k=1.5,acc.period=10.5,shape=2)  )
  
 
  
})


test_that("valid_dropout_input",{
  
  dropout=list(time=-2,proportion=c(0.05,0.05))
  expect_error(CRGIStudy(N=100,study.duration=100,ctrl.time=8,ctrl.proportion=0.5,k=1.5,acc.period=10.5,shape=1.2,
            alpha=0.05,power=0.6,two.sided=TRUE,HR=0.75,r=1.3,followup=10,dropout=dropout))
  
  dropout="boo"
  expect_error(CRGIStudy(N=100,study.duration=100,ctrl.time=8,ctrl.proportion=0.5,k=1.5,acc.period=10.5,shape=1.2,
                         alpha=0.05,power=0.6,two.sided=TRUE,HR=0.75,r=1.3,followup=10,dropout=dropout))
  
  dropout=list(atimes=2,proportion=c(0.05,0.05))
  expect_error(Study(N=100,study.duration=100,ctrl.median=4,k=1.5,acc.period=10.5,shape=1.2,
                         alpha=0.05,power=0.6,two.sided=TRUE,HR=0.75,r=1.3,dropout=dropout))
  
  dropout=list(times=2,proportion=c(1.0,0.5))
  expect_error(Study(N=100,study.duration=100,ctrl.median=4,k=1.5,acc.period=10.5,shape=1.2,
                     alpha=0.05,power=0.6,two.sided=TRUE,HR=0.75,r=1.3,dropout=dropout))
  
  
  dropout=list(times=2,proportion=c(0.8,0.8),shape=-3)
  expect_error(Study(N=100,study.duration=100,ctrl.median=4,k=1.5,acc.period=10.5,shape=1.2,
                     alpha=0.05,power=0.6,two.sided=TRUE,HR=0.75,r=1.3,dropout=dropout))
  
  
  dropout=list(times=2,proportion=c(0.5,0.5),shape=3)
  s <- Study(N=100,study.duration=100,ctrl.median=4,k=1.5,acc.period=10.5,shape=1.2,
             alpha=0.05,power=0.6,two.sided=TRUE,HR=0.75,r=1.3,dropout=dropout)
  
  expect_equal(3,s@dropout.shape)
  expect_equal(2,s@dropout[[1]]@median)
  
  
  dropout=list(times=2,proportion=c(0.75,0.75))
  s <- Study(N=100,study.duration=100,ctrl.median=4,k=1.5,acc.period=10.5,shape=1.2,
             alpha=0.05,power=0.6,two.sided=TRUE,HR=0.75,r=1.3,dropout=dropout)
  
  expect_equal(1,s@dropout.shape)
  expect_equal(1,s@dropout[[1]]@median)
  
  
  
})

