patients <- read.table("/Users/yup111/Documents/courses/stats/project_data.txt", header = FALSE, sep = "", dec = ".")
colnames(patients) <- c("id", "psa_level", "cancer_volume", "weight", "age", "benign_prostatic_hyperplasia", "seminal_vesicle_invasion", "capsular_penetration", "gleason_score")
head(patients)
summary(patients)
round(cor(patients[c("psa_level","cancer_volume","weight","age","benign_prostatic_hyperplasia","seminal_vesicle_invasion","capsular_penetration","gleason_score")]),3) 
patients.model <- lm(psa_level ~ cancer_volume + weight + age + benign_prostatic_hyperplasia + seminal_vesicle_invasion + capsular_penetration + gleason_score, data=patients)
AIC(patients.model)
anova(patients.model)
summary(patients.model)

#suo you model dou yao check

#box-cox
library(MASS)
bc1=boxcox(patients.model)
# lambda=bc1$x[which.max(bc1$y)];lambda
# axis(1,at=round(lambda,2),cex.axis=0.7)
patients.model.r1 <- lm(log(psa_level) ~ cancer_volume + weight + age + benign_prostatic_hyperplasia + seminal_vesicle_invasion + capsular_penetration + gleason_score, data=patients)
source("http://www.stat.ufl.edu/~athienit/check.R")
check(patients.model.r1,tests=TRUE)
summary(patients.model.r1)

# multicollinearity
library(mctest)
allpredictors = patients[,3:9]
omcdiag(allpredictors,patients$psa_level)
imcdiag(allpredictors,patients$psa_level)

#correlation
round(cor(patients[c("psa_level","cancer_volume","seminal_vesicle_invasion","capsular_penetration")]),3) 


#decide to delete capsular_penetration
patients.model.r0 <- lm(log(psa_level) ~ cancer_volume + weight + age + benign_prostatic_hyperplasia + seminal_vesicle_invasion + gleason_score, data=patients)
summary(patients.model.r0)
check(patients.model.r0,tests=TRUE)

# method 1

source("http://www.stat.ufl.edu/~athienit/stepT.R")
patients.model.r2 <- stepT(patients.model.r0,alpha.rem=0.05,direction="backward")
summary(patients.model.r2)

#method 2
patients.model.r3=stepAIC(patients.model.r0,direction="backward")
summary(patients.model.r3)
#leap subset
library(leaps)
patients.model.r4 <- regsubsets(log(psa_level) ~ cancer_volume + weight + age + benign_prostatic_hyperplasia + seminal_vesicle_invasion + gleason_score,
                          nbest=6,data=patients)
aprout <- summary(patients.model.r4)
colnames(patients.model.r4)

with(aprout,round(cbind(which,adjr2,cp,bic),3))     ## Prints "readable" results
plot(patients.model.r4,scale="bic")
plot(patients.model.r4,scale="adjr2")
