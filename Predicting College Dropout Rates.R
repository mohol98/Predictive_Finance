#################################
#### FINC 4355 Assignment 4 #####
#################################

### loading data and packages into memory ###

library(mfx)
library(readxl)
MidusCollege <- read_excel("UALR/FINC4355/Assignments/MidusCollege.xls")
View(MidusCollege)
summary(MidusCollege)

### Linear Probability models ###

RegModel.1 <- lm(col~momedu+race2+sex, data=MidusCollege)
summary(RegModel.1)

RegModel.2 <- lm(col~momedu+race2+sex+paedu+race3, data=MidusCollege)
summary(RegModel.2)

predict(RegModel.2, MidusCollege, type="response")

MidusCollege$predictOLS <- predict(RegModel.2, MidusCollege, type="response")
summary(MidusCollege$predictOLS)

### Logistic Regression ###

### LOGIT ###
GLM.1 <- glm(col~momedu+race2+sex, family=binomial(logit), data=MidusCollege)
summary(GLM.1)
exp(coef(GLM.1))

GLM.2 <- glm(col~momedu+race2+sex+paedu+race3, family=binomial(logit), data=MidusCollege)
summary(GLM.2)
exp(coef(GLM.2))

predict_GLM.2 <- predict(GLM.2, MidusCollege, type="response")
summary(predict_GLM.2)


logitmfx(formula = col~momedu+race2+sex+paedu+race3, data=MidusCollege, atmean = TRUE, robust = FALSE, clustervar1 = NULL,
         clustervar2 = NULL, start = NULL, control = list())

### PROBIT ###

GLM.3 <- glm(col~momedu+race2+sex, family=binomial(probit), data=MidusCollege)
summary(GLM.3)

pnorm(0.21003)
pnorm(0.28962)

probitmfx(formula = Bankruptcy ~ Divorce, data=Dataset, atmean = TRUE, robust = FALSE, clustervar1 = NULL,
          clustervar2 = NULL, start = NULL, control = list())
          
GLM.4 <- glm(col~momedu+race2+sex+paedu+race3, family=binomial(probit), data=MidusCollege)
summary(GLM.4)

predict_GLM.4 <- predict(GLM.4, MidusCollege, type="response")
summary(predict_GLM.4)


