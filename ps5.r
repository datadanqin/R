
#Q1

rm(list=ls())

#Import package
library(data.table)
library(sandwich) 
library(lmtest) 

#data import
context1 <- read.csv("WAGE1.csv")

seed        <-	2	
maxClusters	<-	10

## Use within-group variation to choose k
wss	<- rep(-1,maxClusters)
for (i in 1:maxClusters) { # i represents the k value
  set.seed(seed)
  model <- kmeans(context1,centers=i,nstart=10)
  wss[i] <- model$tot.withinss
}
plot(1:maxClusters,	wss, type="b", 
     xlab="Number of Clusters",
     ylab="Aggregate Within Group SS")


## Run the model
set.seed(seed)
model1 <- kmeans(context1,centers=3,nstart=10)
model1$centers
# > model1$centers
# wage     educ     exper    tenure   nonwhite    female   married    numdep      smsa
# 1 5.900926 10.96296 38.777778 11.657407 0.10185185 0.4629630 0.7500000 0.5740741 0.6388889
# 2 5.206958 13.12548  5.946768  1.699620 0.09885932 0.5133080 0.4486692 0.8897338 0.7870722
# 3 7.105806 12.72258 20.638710  6.316129 0.10967742 0.4322581 0.7806452 1.6322581 0.6709677
# northcen     south      west   construc    ndurman   trcommpu     trade   services  profserv
# 1 0.2962963 0.3333333 0.1574074 0.01851852 0.14814815 0.03703704 0.2222222 0.12037037 0.2500000
# 2 0.2775665 0.3155894 0.1596958 0.06083650 0.07604563 0.05323194 0.3193916 0.09505703 0.2699620
# 3 0.1741935 0.4387097 0.1935484 0.03870968 0.15483871 0.03225806 0.2774194 0.09677419 0.2451613
# profocc   clerocc    servocc
# 1 0.3240741 0.1203704 0.13888889
# 2 0.3498099 0.1863118 0.17110266
# 3 0.4258065 0.1677419 0.09032258

groups1 <- model1$cluster
groups1
context1$cluster <- groups1
table(groups1)

#model2, model3, and model4
model2 <- lm(wage~educ+exper+tenure,data=subset(context1,cluster==1))
model3 <- lm(wage~educ+exper+tenure,data=subset(context1,cluster==2))
model4 <- lm(wage~educ+exper+tenure,data=subset(context1,cluster==3))

summary(model2)
summary(model3)
summary(model4)

# > summary(model2)
# 
# Call:
#   lm(formula = wage ~ educ + exper + tenure, data = context1[clusters == 
#                                                                1])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.4894 -2.0198 -0.5852  1.2467 14.4291 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.78799    2.83502   1.336 0.184419    
# educ         0.40735    0.10276   3.964 0.000135 ***
#   exper       -0.10172    0.05851  -1.739 0.085077 .  
# tenure       0.13653    0.03098   4.407 2.55e-05 ***
#   ---
#   Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
# 
# Residual standard error: 3.292 on 104 degrees of freedom
# Multiple R-squared:  0.3365,	Adjusted R-squared:  0.3174 
# F-statistic: 17.58 on 3 and 104 DF,  p-value: 2.628e-09
# 
# > summary(model3)
# 
# Call:
#   lm(formula = wage ~ educ + exper + tenure, data = context1[clusters == 
#                                                                2])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.7853 -1.2968 -0.3433  0.6621 12.0409 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -3.21053    0.81295  -3.949 0.000101 ***
#   educ         0.52524    0.06025   8.717 3.49e-16 ***
#   exper        0.17286    0.03806   4.542 8.55e-06 ***
#   tenure       0.29156    0.06613   4.409 1.52e-05 ***
#   ---
#   Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
# 
# Residual standard error: 2.278 on 259 degrees of freedom
# Multiple R-squared:  0.3765,	Adjusted R-squared:  0.3693 
# F-statistic: 52.14 on 3 and 259 DF,  p-value: < 2.2e-16
# 
# > summary(model4)
# 
# Call:
#   lm(formula = wage ~ educ + exper + tenure, data = context1[clusters == 
#                                                                3])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -9.6701 -2.2514 -0.5411  1.6947 13.8507 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -4.85740    2.03152  -2.391    0.018 *  
#   educ         0.73631    0.11888   6.194 5.30e-09 ***
#   exper        0.05905    0.05871   1.006    0.316    
# tenure       0.21796    0.04655   4.683 6.26e-06 ***
#   ---
#   Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
# 
# Residual standard error: 3.755 on 151 degrees of freedom
# Multiple R-squared:  0.2939,	Adjusted R-squared:  0.2798 
# F-statistic: 20.95 on 3 and 151 DF,  p-value: 2.118e-11


#a k=3
#bCluster2 has the most long distance from the ducation,Cluster3  has the most short distance from the education.
#Cluster1 has the most long distance from the experience, Cluster3  has the most short distance from the experience.
#Cluster1 has the most long distance from  tenure,Cluster2  has the most short distance from the experience.
#c experience is not significant in model 2 and model 4 but significant in model3.


#Q2

library(sandwich) 
library(lmtest)
library(tseries)
library(plm)

# Import dataset 
context2 <- fread("ffportfolios.csv")
head(context2)

## verifying that every series is level stationary
lapply(context2[,2:ncol(context2)], kpss.test) 
# p-value for all 32 time-series is 0.1; All are level stationary. 

## Run model
model5 <- prcomp(context2[,2:33])
## Generating screeplot
screeplot(model5,type="lines")
model5$rotation[,2]*100

## get the principal components
context2$factor <- model5$x[,1]
head(context2)
summary(context2$factor)

## Setting the variance of factor variable equal to one.

library(expm)
context2$factor <- context2$factor %*% solve(sqrtm(crossprod(context2$factor))) * sqrt(nrow(context2)) 
crossprod(context2$factor)/nrow(context2)
c(mean(context2$factor), var(context2$factor))

##Finding the year values where the standardized factor is less than -2:58.
newdata <- subset(context2, factor<(-2.58))
min(context2$factor)

yearvalues <- cbind(newdata$Year, newdata$Portfolio2, newdata$factor)
yearvalues

# a 1
#b variation

