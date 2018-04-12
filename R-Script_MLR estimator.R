#Hancock Mueller#
HancockMueller<-function(x){
  loads<-inspect(x,"std")$lambda
  facs<-colnames(loads)
  out<-rep(NA,length(facs))
  names(out)<-facs
  
  for(i in facs){
    out[i]<-(1+(1/sum((loads[,i]^2)/(1-loads[,i]^2))))^-1
  }
  out
}

##One-Factor Model 

Modsa1 <- '
ADHD =~ NA*CuSS_S1+CuSS_S2+CuSS_S3+CuSS_S4+CuSS_S5+CuSS_S6+CuSS_S7+CuSS_S8+CuSS_S9+CuSS_S10+CuSS_S11+CuSS_S12+CuSS_S13+CuSS_S14+CuSS_S15+CuSS_S16+CuSS_S17+CuSS_S18
ADHD~~1*ADHD
'
Runsa1 <- cfa(Modsa1, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR, missing="FIML", estimator="MLR")
summary(Runsa1,standardized=TRUE,fit.measures=TRUE, rsquare=TRUE)

#reliability
reliability(Runsa1) 

#Hancock
HancockMueller(Runsa1)

#Measurement invariance: dataset
Runsa1A <- cfa(Modsa1, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR, group="Dataset", missing="FIML", estimator="MLR", std.lv=TRUE)
Runsa1B <- cfa(Modsa1, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR, group="Dataset", group.equal=c("loadings"), missing="FIML", estimator="MLR",std.lv=TRUE)
Runsa1C <- cfa(Modsa1, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR, group="Dataset", group.equal=c("loadings", "intercepts"), missing="FIML", estimator="MLR", std.lv=TRUE)
lavTestLRT(Runsa1A,Runsa1B)
lavTestLRT(Runsa1A,Runsa1C)

#Measurement invariance: Gender
Runsa1AA <- cfa(Modsa1, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR, group="Gender",estimator="MLR", std.lv=TRUE, missing="FIML")
Runsa1BB <- cfa(Modsa1, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR, group="Gender", group.equal=c("loadings"), estimator="MLR", std.lv=TRUE, missing="FIML")
Runsa1CC <- cfa(Modsa1, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR, group="Gender", group.equal=c("loadings", "intercepts"), estimator="MLR", std.lv=TRUE, missing="FIML")
lavTestLRT(Runsa1AA,Runsa1BB)
lavTestLRT(Runsa1AA,Runsa1CC)

##Two-Factor Model

Modsa2<- 
  'INATT=~ NA*CuSS_S1 + CuSS_S3 + CuSS_S5 + CuSS_S7 + CuSS_S9 + CuSS_S11 + CuSS_S13 + CuSS_S15 + CuSS_S17

HYP=~ NA*CuSS_S2 + CuSS_S4 + CuSS_S6 + CuSS_S8 + CuSS_S10 + CuSS_S12 + CuSS_S14 + CuSS_S16 + CuSS_S18

INATT~~1*INATT
HYP~~1*HYP

INATT~~HYP
'

Runsa2 <- cfa(Modsa2, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR, missing="FIML", estimator="MLR")

summary(Runsa2,standardized=TRUE,fit.measures=TRUE, rsquare=TRUE) 

#reliability
reliability(Runsa2)
HancockMueller(Runsa2)

#Measurement invariance: dataset
Runsa2A <- cfa(Modsa2, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR, estimator="MLR", std.lv=TRUE, missing="FIML", group="Dataset")
Runsa2B <- cfa(Modsa2, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR, estimator="MLR", std.lv=TRUE, missing="FIML", group="Dataset", group.equal=c("loadings"))
Runsa2C <- cfa(Modsa2, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR, estimator="MLR", std.lv=TRUE, missing="FIML", group="Dataset", group.equal=c("loadings", "intercepts"))
lavTestLRT(Runsa2A,Runsa2B)
lavTestLRT(Runsa2A,Runsa2C)

#Measurement invariance: Gender
Runsa2AA <- cfa(Modsa2, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR, estimator="MLR", std.lv=TRUE, missing="FIML", group="Gender")
Runsa2BB <- cfa(Modsa2, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR, estimator="MLR", std.lv=TRUE, missing="FIML", group="Gender", group.equal=c("loadings"))
Runsa2CC <- cfa(Modsa2, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR, estimator="MLR", std.lv=TRUE, missing="FIML", group="Gender", group.equal=c("loadings", "intercepts"))
lavTestLRT(Runsa2AA,Runsa2BB)
lavTestLRT(Runsa2AA,Runsa2CC)

##Three-Factor Model

Modsa3<- 
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
Runsa3 <- cfa(Modsa3, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR, missing="FIML", estimator="MLR")
summary(Runsa3,standardized=TRUE,fit.measures=TRUE, rsquare=TRUE) 

#reliability
reliability(Runsa3)
HancockMueller(Runsa3)

#Measurement invariance: dataset
Runsa3A <- cfa(Modsa3, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR,estimator="MLR", std.lv=TRUE, missing="FIML", group="Dataset")
Runsa3B <- cfa(Modsa3, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR,estimator="MLR", std.lv=TRUE, missing="FIML", group="Dataset", group.equal=c("loadings"))
Runsa3C <- cfa(Modsa3, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR,estimator="MLR", std.lv=TRUE, missing="FIML", group="Dataset", group.equal=c("loadings", "intercepts"))
lavTestLRT(Runsa3A,Runsa3B)
lavTestLRT(Runsa3A,Runsa3C)

#Measurement invariance: Gender
Runsa3AA <- cfa(Modsa3, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR,estimator="MLR", std.lv=TRUE, missing="FIML", group="Gender")
Runsa3BB <- cfa(Modsa3, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR,estimator="MLR", std.lv=TRUE, missing="FIML", group="Gender", group.equal=c("loadings"))
Runsa3CC <- cfa(Modsa3, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR,estimator="MLR", std.lv=TRUE, missing="FIML", group="Gender", group.equal=c("loadings", "intercepts"))
lavTestLRT(Runsa3AA,Runsa3BB)
lavTestLRT(Runsa3AA,Runsa3CC)

##Bifactor model (INATT/HYP)

Modsa4 <- '
ADHD =~ CuSS_S1+CuSS_S2+CuSS_S3+CuSS_S4+CuSS_S5+CuSS_S6+CuSS_S7+CuSS_S8+CuSS_S9+CuSS_S10+CuSS_S11+CuSS_S12+CuSS_S13+CuSS_S14+CuSS_S15+CuSS_S16+CuSS_S17+CuSS_S18
INATT=~CuSS_S3 + CuSS_S1 + CuSS_S5 + CuSS_S7 + CuSS_S9 + CuSS_S11 + CuSS_S13 + CuSS_S15 + CuSS_S17
HYP=~CuSS_S2 + CuSS_S4 + CuSS_S6 + CuSS_S8 + CuSS_S10 + CuSS_S12 + CuSS_S14 + CuSS_S16 + CuSS_S18

ADHD~~0*INATT
ADHD~~0*HYP
INATT~~0*HYP
'
Runsa4 <- cfa(Modsa4, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR, estimator="MLR", missing="FIML",std.lv=TRUE)
summary(Runsa4,standardized=TRUE,fit.measures=TRUE, rsquare=TRUE) 

#Reliability
reliability(Runsa4) 
HancockMueller(Runsa4)

#Measurement invariance: dataset
Runsa4A <- cfa(Modsa4, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR,estimator="MLR",  std.lv=TRUE, missing="FIML", group="Dataset")
Runsa4B <- cfa(Modsa4, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR,estimator="MLR", std.lv=TRUE, missing="FIML", group="Dataset", group.equal=c("loadings"))
Runsa4C <- cfa(Modsa4, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR, group="Dataset", group.equal=c("loadings", "intercepts"), missing="FIML", estimator="MLR", std.lv=TRUE)
lavTestLRT(Runsa4A,Runsa4B)
lavTestLRT(Runsa4A,Runsa4C)

#Measurement invariance: Gender
Runsa4AA <- cfa(Modsa4, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR,estimator="MLR",  std.lv=TRUE, missing="FIML", group="Gender")
Runsa4BB <- cfa(Modsa4, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR,estimator="MLR", std.lv=TRUE, missing="FIML", group="Gender", group.equal=c("loadings"))
Runsa4CC <- cfa(Modsa4, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR,estimator="MLR", std.lv=TRUE, missing="FIML", group="Gender", group.equal=c("loadings", "intercepts"))
lavTestLRT(Runsa4AA,Runsa4BB)
lavTestLRT(Runsa4AA,Runsa4CC)

#Explained common variance
L<-inspect(Runsa4, "coef")$lambda
lsq<-L*L
ECV.Runsa4<-sum(lsq[,1])/sum(lsq)

##Bifactor Model-3#

Modsa5 <- '
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
Runsa5 <- cfa(Modsa5, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR,estimator="MLR", missing="FIML",std.lv=TRUE)
summary(Runsa5,standardized=TRUE,fit.measures=TRUE, rsquare=TRUE) 

# Reliability

reliability(Runsa5) 
HancockMueller(Runsa5)

#Explained common variance
M<-inspect(Runsa5, "coef")$lambda
lsq<-M*M
ECV.Runsa5<-sum(lsq[,1])/sum(lsq)


#Measurement invariance: dataset
Runsa5A <- cfa(Modsa5, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR,estimator="MLR",  std.lv=TRUE, missing="FIML", group="Dataset")
Runsa5B <- cfa(Modsa5, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR,estimator="MLR", std.lv=TRUE, missing="FIML", group="Dataset", group.equal=c("loadings"))
Runsa5C <- cfa(Modsa5, dat=Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR, group="Dataset", group.equal=c("loadings", "intercepts"), missing="FIML", estimator="MLR", std.lv=TRUE)
lavTestLRT(Runsa5A,Runsa5B)
lavTestLRT(Runsa5A,Runsa5C)

#Measurement invariance: Gender
Runs5AA <- cfa(Mods5, dat=tempdata2,estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Gender")
Runs5BB <- cfa(Mods5, dat=tempdata2,estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Gender", group.equal=c("loadings"))
Runs5CC <- cfa(Mods5, dat=tempdata2,estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Gender", group.equal=c("loadings", "intercepts"))
lavTestLRT(Runs5AA,Runs5BB)
lavTestLRT(Runs5AA,Runs5CC)
