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

#Reducing dataset to 3 indicators#
tempdata2<-Bifactor_Dataset_Nov_8_17_recodedtocorrectCIHR
sapply(tempdata2[,6:24],table)

tempdata2[which(tempdata2$CuSS_S1==3),]$CuSS_S1<-2
tempdata2[which(tempdata2$CuSS_S2==3),]$CuSS_S2<-2
tempdata2[which(tempdata2$CuSS_S3==3),]$CuSS_S3<-2
tempdata2[which(tempdata2$CuSS_S4==3),]$CuSS_S4<-2
tempdata2[which(tempdata2$CuSS_S5==3),]$CuSS_S5<-2
tempdata2[which(tempdata2$CuSS_S6==3),]$CuSS_S6<-2
tempdata2[which(tempdata2$CuSS_S7==3),]$CuSS_S7<-2
tempdata2[which(tempdata2$CuSS_S8==3),]$CuSS_S8<-2
tempdata2[which(tempdata2$CuSS_S9==3),]$CuSS_S9<-2
tempdata2[which(tempdata2$CuSS_S10==3),]$CuSS_S10<-2
tempdata2[which(tempdata2$CuSS_S11==3),]$CuSS_S11<-2
tempdata2[which(tempdata2$CuSS_S12==3),]$CuSS_S12<-2
tempdata2[which(tempdata2$CuSS_S13==3),]$CuSS_S13<-2
tempdata2[which(tempdata2$CuSS_S14==3),]$CuSS_S14<-2
tempdata2[which(tempdata2$CuSS_S15==3),]$CuSS_S15<-2
tempdata2[which(tempdata2$CuSS_S16==3),]$CuSS_S16<-2
tempdata2[which(tempdata2$CuSS_S17==3),]$CuSS_S17<-2
tempdata2[which(tempdata2$CuSS_S18==3),]$CuSS_S18<-2

##One-Factor Model 

Mods1 <- '
ADHD =~ NA*CuSS_S1+CuSS_S2+CuSS_S3+CuSS_S4+CuSS_S5+CuSS_S6+CuSS_S7+CuSS_S8+CuSS_S9+CuSS_S10+CuSS_S11+CuSS_S12+CuSS_S13+CuSS_S14+CuSS_S15+CuSS_S16+CuSS_S17+CuSS_S18
ADHD~~1*ADHD
'
Runs1 <- cfa(Mods1, dat=tempdata2, missing="pairwise", estimator="WLSMV", zero.add="default", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"))
summary(Runs1,standardized=TRUE,fit.measures=TRUE, rsquare=TRUE)

#reliability
reliability(Runs1) 

#Hancock
HancockMueller(Runs1)

#Measurement invariance: dataset
Runs1A <- cfa(Mods1, dat=tempdata2, group="Dataset", estimator="ULSMV", zero.add=c(0.5,0.5), ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise")
Runs1B <- cfa(Mods1, dat=tempdata2, group="Dataset", group.equal=c("loadings"), missing="pairwise", estimator="ULSMV", zero.add=c(0.5,0.5), ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE)
Runs1C <- cfa(Mods1, dat=tempdata2, group="Dataset", group.equal=c("loadings", "intercepts"), missing="pairwise", estimator="ULSMV", zero.add=c(0.5,0.5), ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE)
lavTestLRT(Runs1A,Runs1B, method="satorra.bentler.2010")
lavTestLRT(Runs1A,Runs1C, method="satorra.bentler.2010")

#Measurement invariance: Gender
Runs1AA <- cfa(Mods1, dat=tempdata2, group="Gender",estimator="ULSMV", zero.add=c(0.5,0.5), ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise")
Runs1BB <- cfa(Mods1, dat=tempdata2, group="Gender", group.equal=c("loadings"), estimator="ULSMV",zero.add=c(0.5,0.5), ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise")
Runs1CC <- cfa(Mods1, dat=tempdata2, group="Gender", group.equal=c("loadings", "intercepts"),zero.add=c(0.5,0.5), estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise")
lavTestLRT(Runs1AA,Runs1BB, method="satorra.bentler.2010")
lavTestLRT(Runs1AA,Runs1CC, method="satorra.bentler.2010")

##Two-Factor Model

Mods2<- 
  'INATT=~ NA*CuSS_S1 + CuSS_S3 + CuSS_S5 + CuSS_S7 + CuSS_S9 + CuSS_S11 + CuSS_S13 + CuSS_S15 + CuSS_S17

HYP=~ NA*CuSS_S2 + CuSS_S4 + CuSS_S6 + CuSS_S8 + CuSS_S10 + CuSS_S12 + CuSS_S14 + CuSS_S16 + CuSS_S18

INATT~~1*INATT
HYP~~1*HYP

INATT~~HYP
'

Runs2 <- cfa(Mods2, dat=tempdata2, estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), zero.add=c(0.5,0.5), std.lv=TRUE, missing="pairwise")

summary(Runs2,standardized=TRUE,fit.measures=TRUE, rsquare=TRUE) 

#reliability
reliability(Runs2)
HancockMueller(Runs2)

#Measurement invariance: dataset
Runs2A <- cfa(Mods2, dat=tempdata2, estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), zero.add=c(0.5,0.5), std.lv=TRUE, missing="pairwise", group="Dataset")
Runs2B <- cfa(Mods2, dat=tempdata2, estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Dataset", zero.add=c(0.5,0.5), group.equal=c("loadings"))
Runs2C <- cfa(Mods2, dat=tempdata2, estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Dataset",zero.add=c(0.5,0.5), group.equal=c("loadings", "intercepts"))
lavTestLRT(Runs2A,Runs2B, method="satorra.bentler.2010")
lavTestLRT(Runs2A,Runs2C, method="satorra.bentler.2010")

#Measurement invariance: Gender
Runs2AA <- cfa(Mods2, dat=tempdata2, estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Gender", zero.add=c(0.5,0.5))
Runs2BB <- cfa(Mods2, dat=tempdata2, estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Gender", group.equal=c("loadings"), zero.add=c(0.5,0.5))
Runs2CC <- cfa(Mods2, dat=tempdata2, estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Gender", group.equal=c("loadings", "intercepts"), zero.add=c(0.5,0.5))
lavTestLRT(Runs2AA,Runs2BB, method="satorra.bentler.2010")
lavTestLRT(Runs2AA,Runs2CC, method="satorra.bentler.2010")

##Three-Factor Model

Mods3<- 
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
Runs3 <- cfa(Mods3, dat=tempdata2, estimator="ULSMV",ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18",std.lv=TRUE, missing="pairwise"),zero.add=c(0.5,0.5))
summary(Runs3,standardized=TRUE,fit.measures=TRUE, rsquare=TRUE) 

#reliability
reliability(Runs3)
HancockMueller(Runs3)

#Measurement invariance: dataset
Runs3A <- cfa(Mods3, dat=tempdata2,estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Dataset",zero.add=c(0.5,0.5))
Runs3Ba <- cfa(Mods3, dat=tempdata2,estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Dataset", group.equal=c("loadings"), zero.add=c(0.5,0.5))
Runs3C <- cfa(Mods3, dat=tempdata2,estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Dataset", group.equal=c("loadings", "intercepts"),zero.add=c(0.5,0.5))
lavTestLRT(Runs3A,Runs3Ba, method="satorra.bentler.2010")
lavTestLRT(Runs3A,Runs3C, method="satorra.bentler.2010")

#Measurement invariance: Gender
Runs3AA <- cfa(Mods3, dat=tempdata2,estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Gender",zero.add=c(0.5,0.5))
Runs3BB <- cfa(Mods3, dat=tempdata2,estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Gender", group.equal=c("loadings"),zero.add=c(0.5,0.5))
Runs3CC <- cfa(Mods3, dat=tempdata2,estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Gender", group.equal=c("loadings", "intercepts"),zero.add=c(0.5,0.5))
lavTestLRT(Runs3AA,Runs3BB,method="satorra.bentler.2010")
lavTestLRT(Runs3AA,Runs3CC,method="satorra.bentler.2010")

##Bifactor model (INATT/HYP)

Mods4 <- '
ADHD =~ CuSS_S1+CuSS_S2+CuSS_S3+CuSS_S4+CuSS_S5+CuSS_S6+CuSS_S7+CuSS_S8+CuSS_S9+CuSS_S10+CuSS_S11+CuSS_S12+CuSS_S13+CuSS_S14+CuSS_S15+CuSS_S16+CuSS_S17+CuSS_S18
INATT=~CuSS_S3 + CuSS_S1 + CuSS_S5 + CuSS_S7 + CuSS_S9 + CuSS_S11 + CuSS_S13 + CuSS_S15 + CuSS_S17
HYP=~CuSS_S2 + CuSS_S4 + CuSS_S6 + CuSS_S8 + CuSS_S10 + CuSS_S12 + CuSS_S14 + CuSS_S16 + CuSS_S18

ADHD~~0*INATT
ADHD~~0*HYP
INATT~~0*HYP
'
Runs4 <- cfa(Mods4, dat=tempdata2, estimator="ULSMV",ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"),std.lv=TRUE,zero.add=c(0.5,0.5))
summary(Runs4,standardized=TRUE,fit.measures=TRUE, rsquare=TRUE) 

#Reliability
reliability(Runs4) 
HancockMueller(Runs4)

#Measurement invariance: dataset
Runs4A <- cfa(Mods4, dat=tempdata2,estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Dataset",zero.add=c(0.5,0.5))
Runs4B <- cfa(Mods4, dat=tempdata2,estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Dataset", group.equal=c("loadings"),zero.add=c(0.5,0.5))
Runs4C <- cfa(Mods4, dat=tempdata2, group="Dataset", group.equal=c("loadings", "intercepts"), missing="pairwise", estimator="ULSMV", zero.add=c(0.5,0.5), ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE,zero.add=c(0.5,0.5))
lavTestLRT(Runs4A,Runs4B,method="satorra.bentler.2010")
lavTestLRT(Runs4A,Runs4C,method="satorra.bentler.2010")

#Measurement invariance: Gender
Runs4AA <- cfa(Mods4, dat=tempdata2,estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Gender",zero.add=c(0.5,0.5))
Runs4BB <- cfa(Mods4, dat=tempdata2,estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Gender", group.equal=c("loadings"),zero.add=c(0.5,0.5))
Runs4CC <- cfa(Mods4, dat=tempdata2,estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Gender", group.equal=c("loadings", "intercepts"),zero.add=c(0.5,0.5))
lavTestLRT(Runs4AA,Runs4BB,method="satorra.bentler.2010")
lavTestLRT(Runs4AA,Runs4CC,method="satorra.bentler.2010")

#Explained common variance
L<-inspect(Runs4, "coef")$lambda
lsq<-L*L
ECV.Run4<-sum(lsq[,1])/sum(lsq)

##Bifactor Model-3#

Mods5 <- '
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
Runs5 <- cfa(Mods5, dat=tempdata2,estimator="ULSMV", std.lv=TRUE, ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), missing ="pairwise", zero.add=c(0.5,0.5))
summary(Runs5,standardized=TRUE,fit.measures=TRUE, rsquare=TRUE) 

# Reliability

reliability(Runs5) 
HancockMueller(Runs5)

#Explained common variance
M<-inspect(Runs5, "coef")$lambda
lsq<-M*M
ECV.Run5<-sum(lsq[,1])/sum(lsq)


#Measurement invariance: dataset
Runs5A <- cfa(Mods5, dat=tempdata2,estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Dataset",zero.add=c(0.5,0.5))
Runs5B <- cfa(Mods5, dat=tempdata2,estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Dataset", group.equal=c("loadings"),zero.add=c(0.5,0.5))
Runs5C <- cfa(Mods5, dat=tempdata2, group="Dataset", group.equal=c("loadings", "intercepts"), missing="pairwise", estimator="ULSMV", zero.add=c(0.5,0.5), ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE,zero.add=c(0.5,0.5))
lavTestLRT(Runs5A,Runs5B, method="satorra.bentler.2010")
lavTestLRT(Runs5A,Runs5C, method="satorra.bentler.2010")

#Measurement invariance: Gender
Runs5AA <- cfa(Mods5, dat=tempdata2,estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Gender",zero.add=c(0.5,0.5))
Runs5BB <- cfa(Mods5, dat=tempdata2,estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Gender", group.equal=c("loadings"),zero.add=c(0.5,0.5))
Runs5CC <- cfa(Mods5, dat=tempdata2,estimator="ULSMV", ordered=c("CuSS_S1", "CuSS_S2","CuSS_S3", "CuSS_S4", "CuSS_S5", "CuSS_S6", "CuSS_S7", "CuSS_S8", "CuSS_S9", "CuSS_S10", "CuSS_S11", "CuSS_S12", "CuSS_S13", "CuSS_S14", "CuSS_S15", "CuSS_S16", "CuSS_S17", "CuSS_S18"), std.lv=TRUE, missing="pairwise", group="Gender", group.equal=c("loadings", "intercepts"),zero.add=c(0.5,0.5))
lavTestLRT(Runs5AA,Runs5BB, method="satorra.bentler.2010")
lavTestLRT(Runs5AA,Runs5CC, method="satorra.bentler.2010")
