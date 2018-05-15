Modsa3V<- 
  'INATT=~ NA*CuSS_S1 + CuSS_S3 + CuSS_S5 + CuSS_S7 + CuSS_S9 + CuSS_S11 + CuSS_S13 + CuSS_S15 + CuSS_S17

IMP=~ NA*CuSS_S14 + CuSS_S16 + CuSS_S18

HYP=~ NA*CuSS_S2 + CuSS_S4 + CuSS_S6 + CuSS_S8 + CuSS_S10 + CuSS_S12
INATT~~1*INATT
IMP~~1*IMP
HYP~~1*HYP
INATT~~HYP
IMP~~INATT
HYP~~IMP
Education ~ INATT+IMP+HYP
BSI_Hostility ~ INATT+IMP+HYP
BSI_Depression ~ INATT+IMP+HYP
APQ_PPComp ~ INATT+IMP+HYP
SELF_APQ_ID ~ INATT+IMP+HYP

'
Runsa3V <- cfa(Modsa3V, dat=BifactorDataset_APQcomposite, missing="FIML", estimator="MLR")
summary(Runsa3V,standardized=TRUE,fit.measures=TRUE, rsquare=TRUE) 



Modsa4V <- '
ADHD =~ CuSS_S1+CuSS_S2+CuSS_S3+CuSS_S4+CuSS_S5+CuSS_S6+CuSS_S7+CuSS_S8+CuSS_S9+CuSS_S10+CuSS_S11+CuSS_S12+CuSS_S13+CuSS_S14+CuSS_S15+CuSS_S16+CuSS_S17+CuSS_S18
INATT=~CuSS_S3 + CuSS_S1 + CuSS_S5 + CuSS_S7 + CuSS_S9 + CuSS_S11 + CuSS_S13 + CuSS_S15 + CuSS_S17
HYP=~CuSS_S2 + CuSS_S4 + CuSS_S6 + CuSS_S8 + CuSS_S10 + CuSS_S12 + CuSS_S14+CuSS_S16+CuSS_S18



ADHD~~0*INATT
ADHD~~0*HYP
INATT~~0*HYP

Education ~ ADHD+INATT+HYP
BSI_Hostility ~ ADHD+INATT+HYP
BSI_Depression ~ ADHD+INATT+HYP
APQ_PPComp ~ ADHD+INATT+HYP
SELF_APQ_ID ~ ADHD+INATT+HYP

'
Runsa4V <- cfa(Modsa4V, dat=BifactorDataset_APQcomposite,estimator="MLR", missing="FIML",std.lv=TRUE)
summary(Runsa4V,standardized=TRUE,fit.measures=TRUE, rsquare=TRUE) 


Modsa5V <- '
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

Education ~ INATT+IMP+HYP+ADHD
BSI_Hostility ~ INATT+IMP+HYP+ADHD
BSI_Depression ~ INATT+IMP+HYP+ADHD
APQ_PPComp ~ INATT+IMP+HYP+ADHD
SELF_APQ_ID ~ INATT+IMP+HYP+ADHD

'
Runsa5V <- cfa(Modsa5V, dat=BifactorDataset_APQcomposite,estimator="MLR", missing="FIML",std.lv=TRUE)
summary(Runsa5V,standardized=TRUE,fit.measures=TRUE, rsquare=TRUE) 