library(lavaan)

#file.choose()
dataset<-read.csv("Bifactor_April2718.csv", header=TRUE)



#Model 1 - This is the 3-factor model, to test whether this model is nested within Bifactor 2 and Bifactor 3#
Model1<- 
  'INATT=~ NA*CuSS_S1 + CuSS_S3 + CuSS_S5 + CuSS_S7 + CuSS_S9 + CuSS_S11 + CuSS_S13 + CuSS_S15 + CuSS_S17

IMP=~ NA*CuSS_S14 + CuSS_S16 + CuSS_S18

HYP=~ NA*CuSS_S2 + CuSS_S4 + CuSS_S6 + CuSS_S8 + CuSS_S10 + CuSS_S12
INATT~~1*INATT
IMP~~1*IMP
HYP~~1*HYP
INATT~~HYP
IMP~~INATT
HYP~~IMP
'
Run1 <- cfa(Model1, dat=dataset, estimator="MLR", missing="FIML",std.lv=TRUE)
summary(Run1,standardized=TRUE,fit.measures=TRUE, rsquare=TRUE) 


#Model 2 - This is the bifactor 2 model#

Model2 <- '
ADHD =~ CuSS_S1+CuSS_S2+CuSS_S3+CuSS_S4+CuSS_S5+CuSS_S6+CuSS_S7+CuSS_S8+CuSS_S9+CuSS_S10+CuSS_S11+CuSS_S12+CuSS_S13+CuSS_S14+CuSS_S15+CuSS_S16+CuSS_S17+CuSS_S18
INATT=~CuSS_S3 + CuSS_S1 + CuSS_S5 + CuSS_S7 + CuSS_S9 + CuSS_S11 + CuSS_S13 + CuSS_S15 + CuSS_S17
HYP=~CuSS_S2 + CuSS_S4 + CuSS_S6 + CuSS_S8 + CuSS_S10 + CuSS_S12 + CuSS_S14 + CuSS_S16 + CuSS_S18

ADHD~~0*INATT
ADHD~~0*HYP
INATT~~0*HYP
'
Run2 <- cfa(Model2, dat=dataset, estimator="MLR", missing="FIML",std.lv=TRUE)
summary(Run2,standardized=TRUE,fit.measures=TRUE, rsquare=TRUE) 




# Model 3 - This is the bifactor 3 model#

Model3 <- '
ADHD =~ CuSS_S1+CuSS_S2+CuSS_S3+CuSS_S4+CuSS_S5+CuSS_S6+CuSS_S7+CuSS_S8+CuSS_S9+CuSS_S10+CuSS_S11+CuSS_S12+CuSS_S13+CuSS_S14+CuSS_S15+CuSS_S16+CuSS_S17+CuSS_S18
INATT=~CuSS_S3 + CuSS_S1 + CuSS_S5 + CuSS_S7 + CuSS_S9 + CuSS_S11 + CuSS_S13 + CuSS_S15 + CuSS_S17
HYP=~CuSS_S2 + CuSS_S4 + CuSS_S6 + CuSS_S8 + CuSS_S10 + CuSS_S12 
IMP=~CuSS_S14+CuSS_S16+CuSS_S18


ADHD~~0*INATT
ADHD~~0*HYP
ADHD~~0*IMP
INATT~~0*HYP
INATT~~0*IMP
HYP~~0*IMP

'
Run3 <- cfa(Model3, dat=dataset,estimator="MLR", missing="FIML",std.lv=TRUE)
summary(Run3,standardized=TRUE,fit.measures=TRUE, rsquare=TRUE) 


#----NET TEST 1: Is Model 2 nested within Model 1?-----------------

#Model-implied covariance matrix from Model 1 run: 
Sigma.Model1<-fitted.values(Run1)$cov   

#Model 2 fitted to the model-implied covariance matrix from Model 1 run:
Run2.nettest21 <- cfa(Model2, sample.cov=Sigma.Model1, sample.nobs=430, std.lv=TRUE) #N doesn't matter
summary(Run2.nettest21) 

#You see that the fit is perfect--these models are nested.

# > summary(Run2.nettest21)
# lavaan (0.5-23.1097) converged normally after  45 iterations
# 
# Number of observations                           430
# 
# Estimator                                         ML
# Minimum Function Test Statistic                0.000
# Degrees of freedom                               117
# P-value (Chi-square)                           1.000

#----NET TEST 2: Is Model 3 nested within Model 1?-----------------

#Model-implied covariance matrix from Model 1 run: 
Sigma.Model1<-fitted.values(Run1)$cov   

#Model 3 fitted to the model-implied covariance matrix from Model 1 run:
Run3.nettest31 <- cfa(Model3, sample.cov=Sigma.Model1, sample.nobs=430, std.lv=TRUE) #N doesn't matter
summary(Run3.nettest31) 

#You see that the fit is perfect--these models are also nested.
# > summary(Run3.nettest31)
# lavaan (0.5-23.1097) converged normally after  53 iterations
# 
# Number of observations                           430
# 
# Estimator                                         ML
# Minimum Function Test Statistic                0.000
# Degrees of freedom                               117
# P-value (Chi-square)                           1.000


