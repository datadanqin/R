###############################################################
# Title:        ps3.r
# Author:       Danqin Shang
# Date:         2017-10-24
###############################################################
rm(list=ls(all=TRUE))
library(data.table)
library(sandwich)
library(lmtest)
library(plm)
library(tseries)
library(DataCombine)
#Question 1
context1 <- fread('hprice1.csv')
summary(context1)
model1 <- lm(price~bdrms + lotsize+sqrft, data=context1)
coeftest(model1)
#> coeftest(model1)

#t test of coefficients:
  
#  Estimate  Std. Error t value  Pr(>|t|)    
#(Intercept) -2.1770e+01  2.9475e+01 -0.7386  0.462208    
#bdrms        1.3853e+01  9.0101e+00  1.5374  0.127945    
#lotsize      2.0677e-03  6.4213e-04  3.2201  0.001823 ** 
#  sqrft        1.2278e-01  1.3237e-02  9.2751 1.658e-14 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
coeftest(model1,vcov. = vcovHC)
#> coeftest(model1,vcov. = vcovHC)

#t test of coefficients:
  
#  Estimate  Std. Error t value Pr(>|t|)   
#(Intercept) -21.7703086  41.0326944 -0.5306 0.597124   
#bdrms        13.8525219  11.5617901  1.1981 0.234236   
#lotsize       0.0020677   0.0071485  0.2893 0.773101   
#sqrft         0.1227782   0.0407325  3.0143 0.003406 **
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
model2 <- lm(log(price)~bdrms + log(lotsize)+log(sqrft),data=context1)
coeftest(model2)
#> coeftest(model2)

#t test of coefficients:
  
#  Estimate Std. Error t value  Pr(>|t|)    
#(Intercept)  -1.297042   0.651284 -1.9915   0.04967 *  
#  bdrms         0.036958   0.027531  1.3424   0.18308    
#log(lotsize)  0.167967   0.038281  4.3877 3.307e-05 ***
#  log(sqrft)    0.700232   0.092865  7.5403 5.006e-11 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
coeftest(model2,vcov. = vcovHC)
#> coeftest(model2,vcov. = vcovHC)

#t test of coefficients:
  
#  Estimate Std. Error t value  Pr(>|t|)    
#(Intercept)  -1.297042   0.850457 -1.5251  0.130988    
#bdrms         0.036958   0.035576  1.0389  0.301845    
#log(lotsize)  0.167967   0.053275  3.1528  0.002243 ** 
#  log(sqrft)    0.700232   0.121392  5.7683 1.298e-07 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#a.lotsize is significant on 1% level. sqrft is significant on 0.1% level.
#b.sqrft is still significant.
#c.log(lotsize) and log(sqrft) are significant.
#d.log(lotsize) and log(sqrft) are significant.
#e.Using log transformation rescale the variables which result in smaller residual errors
# and thus decrease heteroskedascity, so it is a good way to correct heteroskedasticity. 
# But we should only use it if all values are positive and the new model provides 
# a reasonable interpretation.

#Question 2
context2 <- fread('beveridge.csv')
model3 <- lm(urate~vrate, data=context2)
coeftest(model3)
#> coeftest(model3)

#t test of coefficients:
  
#  Estimate Std. Error t value  Pr(>|t|)    
#(Intercept) 17.11942    0.59200  28.918 < 2.2e-16 ***
#  vrate       -3.74145    0.20681 -18.091 < 2.2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
coeftest(model3,vcov.=NeweyWest(model3,lag=5))
#> coeftest(model3,vcov.=NeweyWest(model3,lag=5))

#t test of coefficients:
  
#  Estimate Std. Error t value  Pr(>|t|)    
#(Intercept) 17.11942    1.36561  12.536 < 2.2e-16 ***
#  vrate       -3.74145    0.39575  -9.454 < 2.2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
kpss.test(context2$urate,null="Level")
# > kpss.test(context2$urate,null="Level")

#KPSS Test for Level Stationarity

#data:  context2$urate
#KPSS Level = 2.6835, Truncation lag parameter = 2, p-value = 0.01
kpss.test(context2$urate,null="Trend")
#p-value = 0.01
kpss.test(context2$vrate,null="Level")
#p-value = 0.01
kpss.test(context2$vrate,null="Trend")
#p-value = 0.01

kpss.test(diff(context2$urate),null="Level")
#p-value = 0.1 
kpss.test(diff(context2$urate),null="Trend")
#p-value = 0.01
kpss.test(diff(context2$vrate),null="Level")
#p-value = 0.1
kpss.test(diff(context2$vrate),null="Trend")
#p-value = 0.01245

kpss.test(diff(diff(context2$urate)),null="Level")
#p-value = 0.1 
kpss.test(diff(diff(context2$urate)),null="Trend")
#p-value = 0.1
kpss.test(diff(diff(context2$vrate)),null="Level")
#p-value = 0.1
kpss.test(diff(diff(context2$vrate)),null="Trend")
#p-value = 0.1

model4 <- lm(diff(urate)~ diff(vrate),data=context2)

coeftest(model4)
#> coeftest(model4)

#t test of coefficients:
  
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)  0.037046   0.017809  2.0802  0.03944 *
#  diff(vrate) -0.027599   0.107318 -0.2572  0.79745  
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
coeftest(model4, vcov.=NeweyWest(model4,lag=5))
#> coeftest(model4, vcov.=NeweyWest(model4,lag=5))

#t test of coefficients:
  
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)  0.037046   0.030041  1.2332   0.2197
#diff(vrate) -0.027599   0.081122 -0.3402   0.7342

#f. OLS and NeweyWest tests show that the coefficient on the vanancy rate is significant.
#g. Based on the KPSS findings, we should do first differencing to apply to urate.
#h. we should do first differencing to vrate.
#i. The coefficient of vrate is not sinificant in model 4 anymore.
#j. Model 4 better decribes the data.

#Question 3
context3 <- fread('JTRAIN.csv')

#Generate lag(grant)
#context3$grant_lag <- rep(0,471)
#for (i in nrow(context3))
#  if(context3$year[i]!=1987)
#    context3$grant_lag[i]<- context3$grant[i-1]
for(i in 1:nrow(context3)){
  if(i%%3 == 1)context3$grant_1[i] <- 0
  else context3$grant_1[i] <- context3$grant[i-1]}
d88 <-ifelse(context3$year == 1988, 1,0)
d89 <-ifelse(context3$year == 1989, 1,0)

#declare panel
context3 <- plm.data(context3,indexes=c('fcode','year'))
#context3 <-slide(context3, Var = 'grant', NewVar = 'grant_lag',slideBy = -1)
model5 <- plm(log(scrap)~d88+d89+grant+grant_1, data = context3, model = 'pooling')
coeftest(model5)
#> coeftest(model5)

#t test of coefficients:
  
#Estimate Std. Error t value Pr(>|t|)   
#(Intercept)  0.597434   0.203063  2.9421 0.003754 **
#  d88         -0.239370   0.310864 -0.7700 0.442447   
#d89         -0.496524   0.337928 -1.4693 0.143748   
#grant        0.200020   0.338285  0.5913 0.555186   
#grant_1      0.048936   0.436066  0.1122 0.910792
model6 <- plm(log(scrap)~d88+d89+grant+grant_1, data = context3, model = 'within')
coeftest(model6)
#> coeftest(model6)

#t test of coefficients:
  
#Estimate Std. Error t value Pr(>|t|)  
#d88     -0.080216   0.109475 -0.7327  0.46537  
#d89     -0.247203   0.133218 -1.8556  0.06634 .
#grant   -0.252315   0.150629 -1.6751  0.09692 .
#grant_1 -0.421590   0.210200 -2.0057  0.04749 *
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
coeftest(model6, vcov. = vcovHC(model6, method='arellano'))
#> coeftest(model6, vcov. = vcovHC(model6, method='arellano'))

#t test of coefficients:
  
#Estimate Std. Error t value Pr(>|t|)  
#d88     -0.080216   0.095719 -0.8380  0.40393  
#d89     -0.247203   0.192514 -1.2841  0.20197  
#grant   -0.252315   0.140329 -1.7980  0.07507 .
#grant_1 -0.421590   0.276335 -1.5256  0.13013  
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#k. if workers were given a grant, the scrap rate will increase 9.99%.
#l. if a firm had a grant last year, the scrap rate will increase 0.148%.
#m. the signs are positive while means both grant and grant_1 are positively correlated
#   with scrap rate.
#n. if a firm has a grant this year, the scrap rate will increase 22.35%.
#o. if a firm has a grant last year, the scrap rate will increase 19.46%.
#p. Still potivily correalted.
#q. They are still not significant.
