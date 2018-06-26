###############################################################
# Title:        ps4.r
# Author:       Danqin Shang
# Date:         2017-11-24
###############################################################
rm(list=ls())
library(data.table)
library(MASS)
library(sandwich)

library(evtree)
library(party)
library(rpart)
#Question 1
context1 <- fread('htv.csv')
model1 <- lm(log(wage)~abil+educ+exper, data=context1)
summary(model1)

#Find the AIC and BIC of model1.
AIC(model1)
#> AIC(model1)
#[1] 1935.995
BIC(model1)
#> BIC(model1)
#[1] 1961.569

model2 <- lm(log(wage)~abil+educ+exper+I(abil^2)+I(educ^2)+I(exper^2)+abil:educ+abil:exper+educ:exper, data = context1)
summary(model2)
AIC(model2)
#> AIC(model2)
#[1] 1926.256
BIC(model2)
#> BIC(model2)
#[1] 1982.518

install.packages("leaps")
library(leaps)
install.packages("MASS")
library(MASS)
formula <- log(wage) ~ abil + educ + exper + I(abil^2) + I(educ^2) + I(exper^2) + (abil:educ) + (abil:exper) + (educ:exper)
model_test <- regsubsets(formula, data = context1, nvmax = 10)
summary(model_test)


#Model selection
model25 <- step(model2,direction = 'backward',k=log(nrow(context1)))
modelx <- lm(log(wage)~abil+educ+exper+educ:exper)
summary(modelx)
BIC(modelx)


#a. BIC of model 1 is slightly lower than revised model2, but the R-squared is improved.
#b. the variable (educi * experi) measures the amount by which the change in response with educ or exper is affected by the other predictor.
#   Their coefficient is significant, so we can say that the change in response with one varible is depends on the value of the other one.

#Question 2
context2 <- fread('loanapp.csv')
model3 <- glm(approve~white,family=binomial(link="logit"),data = context2)
summary(model3)
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   0.8847     0.1253   7.061 1.65e-12 ***
#  white         1.4094     0.1512   9.325  < 2e-16 ***

#White heteroskedasticity robust standard errors for model 3
coeftest(model3,vcov. = vcovHC)
sqrt(diag(vcovHC(model3)))
#> sqrt(diag(vcovHC(model3)))
#(Intercept)       white 
#0.1257008   0.1515177 

model4 <- glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist
              +pubrec+mortlat1+mortlat2+vr, family = binomial(link="logit"),data = context2)
summary(model4)

#White heteroskedasticity robust standard errors for model 4
coeftest(model4,vcov.=vcovHC)
sqrt(diag(vcovHC(model4)))
#> sqrt(diag(vcovHC(model4)))
#(Intercept)       white        hrat       obrat     loanprc        unem        male     married         dep 
#0.65395253  0.17776418  0.01392439  0.01280931  0.53516018  0.03612441  0.21017373  0.18685748  0.07541176 
#sch      cosign       chist      pubrec    mortlat1    mortlat2          vr 
#0.17902371  0.40679430  0.17326490  0.23307622  0.54559973  0.60899502  0.15665300 

model5 <- glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist
              +pubrec+mortlat1+mortlat2+vr+white:obrat, family = binomial(link="logit"),data = context2)
summary(model5)

#White heteroskedasticity robust standard errors for model 5
sqrt(diag(vcovHC(model5)))
#> sqrt(diag(vcovHC(model5)))
#(Intercept)       white        hrat       obrat     loanprc        unem        male     married         dep 
#0.90135483  0.85877152  0.01415830  0.02072761  0.53354587  0.03598809  0.21084173  0.18721204  0.07602953 
#sch      cosign       chist      pubrec    mortlat1    mortlat2          vr white:obrat 
#0.18080730  0.41300292  0.17415405  0.23507486  0.54040113  0.61001334  0.15730199  0.02360318 

#a. The coefficient indicating roughly that an increase in the white is associated with an increase in the probability
#   of approve.
#b. it is still significant, but the value of coefficient decreases.
#c. white is not significant anymore
#d. it measures the amount by which the change in approve with white or obrat is affected by the other predictor.

#Question 3
context3 <- fread('smoke.csv')
model6 <- glm(cigs~educ+age+I(age^2)+log(income)+restaurn,family = poisson(link = "log"),data=context3)
summary(model6)
#> summary(model6)

#Call:
#  glm(formula = cigs ~ educ + age + I(age^2) + log(income) + restaurn, 
#      family = poisson(link = "log"), data = context3)

#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-6.338  -4.229  -3.280   2.223  13.942  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#  (Intercept) -8.953e-02  1.881e-01  -0.476    0.634    
#  educ        -5.952e-02  4.257e-03 -13.981  < 2e-16 ***
#  age          1.140e-01  4.968e-03  22.943  < 2e-16 ***
#  I(age^2)    -1.368e-03  5.696e-05 -24.016  < 2e-16 ***
#  log(income)  1.047e-01  2.026e-02   5.168 2.36e-07 ***
#  restaurn    -3.613e-01  3.074e-02 -11.754  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#(Dispersion parameter for poisson family taken to be 1)

#Null deviance: 15821  on 806  degrees of freedom
#Residual deviance: 14755  on 801  degrees of freedom
#AIC: 16238

#Number of Fisher Scoring iterations: 6

sqrt(diag(vcovHC(model6)))
#> sqrt(diag(vcovHC(model6)))
#(Intercept)         educ          age     I(age^2)  log(income)     restaurn 
#0.7819304673 0.0194111306 0.0215661893 0.0002485045 0.0840806644 0.1386491201 

#a. exp(-0.0595)=0.942. Therefore, if the educ increase by 1 year, the cigs will decrease by a factor of 0.942. 
#b. when a person is 20, the rate of change for ln[cigs] is 0.0579. when a person is 60, it is -0.0515. 

#Question 4
context4 <- fread('hdisease.csv')
context4$exang1 <-	ifelse(context4$exang=="yes",1,0)
model7 <- evtree(hdisease~age+cp+trestbps+thalach+exang1,data=context4)
model8 <- ctree(hdisease~age+cp+trestbps+thalach+exang1,data=context4)
plot(model7)
plot(model8)
context5 <- fread('hdisease-new.csv')
names(context5)
context5$exang1 <- ifelse(context5$exang=="yes",1,0)
context5$hdisease_pred <- predict(model8,context5[,c(-1,-6)],type="response")
head(context5)
#> head(context5)
#dset age cp trestbps thalach exang exang1 hdisease_pred
#1: Dallas  62  3      154     139   yes      1     0.7073171
#2: Dallas  47  3      116     149   yes      1     0.7073171
#3: Dallas  61  4      134     130    no      0     1.4086022
#4: Dallas  44  3      129     126    no      0     0.1095890
#5: Dallas  62  2       93     130   yes      1     0.7073171
#6: Dallas  58  4      131     107    no      0     1.4086022

#a.model 7 may be underfitting the data, model 8 may be over fitting the data.
#b. dset is a constant categorical data that describe the location of data which is 'dallas'.