####################################
###### FINC 4355 Assignment 3 ######
###### Quantile Regression #########
####################################

#Install and Load relevant packages into the memory

install.packages("quantreg",dependencies=TRUE)
install.packages("corrgram",dependencies=TRUE)

library("quantreg")
library("corrgram")

#Load Ames Data into memory

library(readxl)
Housing <- read_excel("UALR/FINC4355/Data/AmesInput.xlsx")
View(AmesInput)

# Messy Correlogram for fun

corrgram(Housing, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Housing Correlations")

#1. OLS Regression

RegModel <- lm(saleprice~bedroomabvgr+fullbath+lotarea+exterqual1+exterqual2+exterqual3, data=Housing)
summary(RegModel)
anova(RegModel)
AIC(RegModel)

#2. Quantile Regression 50th %

QTRModel.1 <- rq(saleprice~bedroomabvgr+fullbath+lotarea+exterqual1+exterqual2+exterqual3, data=Housing, tau=.50)
summary(QTRModel.1)
AIC(QTRModel.1)

#3. Quantile Regression 80th %

QTRModel.2 <- rq(saleprice~bedroomabvgr+fullbath+lotarea+exterqual1+exterqual2+exterqual3, data=Housing, tau=.8)
summary(QTRModel.2)
AIC(QTRModel.2)

#4. Quantile Regression 20th %

QTRModel.3 <- rq(saleprice~bedroomabvgr+fullbath+lotarea+exterqual1+exterqual2+exterqual3, data=Housing, tau=.2)
summary(QTRModel.3)
AIC(QTRModel.3)

#5. Quantile Coefficients Plot

QR= rq(saleprice ~ bedroomabvgr+fullbath+lotarea+exterqual1+exterqual2+exterqual3, data=Housing, tau=seq(0.1, 0.9, by=0.1))
sumQR=summary(QR)
plot(sumQR)