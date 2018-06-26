###############################################################
# Title:        ps2.r
# Author:       Danqin Shang
# Date:         2017-09-26
###############################################################
#Question 1
rm(list=ls(all=TRUE))
library(data.table)

context1 <- fread('attend.csv')
summary(context1)
#> summary(context1)
#attend         termGPA     
#Min.   : 2.00   Min.   :0.000  
#1st Qu.:24.00   1st Qu.:2.138  
#Median :28.00   Median :2.670  
#Mean   :26.15   Mean   :2.601  
#3rd Qu.:30.00   3rd Qu.:3.120  
#Max.   :32.00   Max.   :4.000  
#priGPA           ACT       
#Min.   :0.857   Min.   :13.00  
#1st Qu.:2.190   1st Qu.:20.00  
#Median :2.560   Median :22.00  
#Mean   :2.587   Mean   :22.51  
#3rd Qu.:2.942   3rd Qu.:25.00  
#Max.   :3.930   Max.   :32.00  
#final           frosh       
#Min.   :10.00   Min.   :0.0000  
#1st Qu.:22.00   1st Qu.:0.0000  
#Median :26.00   Median :0.0000  
#Mean   :25.89   Mean   :0.2324  
#3rd Qu.:29.00   3rd Qu.:0.0000  
##soph              hw       
#Min.   :0.0000   Min.   :0.000  
#1st Qu.:0.0000   1st Qu.:7.000  
#Median :1.0000   Median :8.000  
#Mean   :0.5765   Mean   :6.971  
#3rd Qu.:1.0000   3rd Qu.:8.000  
#Max.   :1.0000   Max.   :8.000  

attendrt <- context1$attend / 32
hwrt <- context1$hw / 8

model1 <- lm(termGPA~priGPA + ACT +attendrt+hwrt, data=context1) 

summary(model1)
#> summary(model1)

#Call:
#  lm(formula = termGPA ~ priGPA + ACT + attendrt + hwrt, data = context1)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.87210 -0.28100  0.00001  0.30164  1.49711 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.286983   0.164169  -7.839 1.77e-14 ***
#  priGPA       0.548962   0.042418  12.942  < 2e-16 ***
#  ACT          0.036099   0.006051   5.966 3.92e-09 ***
#  attendrt     1.052246   0.155436   6.770 2.81e-11 ***
#  hwrt         0.913031   0.116932   7.808 2.22e-14 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.4788 on 675 degrees of freedom
#Multiple R-squared:   0.58,	Adjusted R-squared:  0.5775 
#F-statistic:   233 on 4 and 675 DF,  p-value: < 2.2e-16
  
#a. Every 10% increase in the attendance is associated with 0.105 increase in the term GPA controlling for prior GPA, ACT score, and homework completion.
#b. Every 10% increase in the homework turned in is associated with 0.091 increase in the term GPA.
#c. 
termGPAc = -1.287 +0.549*2.2+0.036*32+1.052*(28/32) +0.913*1
#> termGPAc
#[1] 2.9063
# The termGPA for such a student described in question c would have a GPA of 2.91
#d.
termGPAd = -1.287 +0.549*3.9+0.036*20+1.052*(28/32) +0.913*1
#> termGPAd
#[1] 3.4076
# The termGPA for such a student described in question d would have a GPA of 3.41
#e. Intuitively, priGPA is more important to the term GPA than ACT. 
#f.
termGPAf = -1.287 +0.549*3.0+0.036*25+1.052*1 +0.913*0.5
#> termGPAf
#[1] 2.7685
# The termGPA for such a student described in question f would have a GPA of 2.77
#g.
termGPAg = -1.287 +0.549*3.0+0.036*25+1.052*0.5 +0.913*1
#> termGPAg
#[1] 2.699
# The termGPA for such a student described in question g would have a GPA of 2.70
#h.Intuitively, attendence is a little more important for termGPA than homework completion.
#i.attendrt and hwrt have the same scale,so their coefficients are easier to compare while PriGPA and ACT have different range of values.
#   attendence and homework completion are measured in rates.


#Question 2
context2 <- fread('CEOSAL2.csv')
summary(context2)
#> summary(context2)
#salary            age       
#Min.   : 100.0   Min.   :33.00  
#1st Qu.: 471.0   1st Qu.:52.00  
#Median : 707.0   Median :57.00  
#Mean   : 865.9   Mean   :56.43  
#3rd Qu.:1119.0   3rd Qu.:62.00  
#Max.   :5299.0   Max.   :86.00  
#college            grad       
#Min.   :0.0000   Min.   :0.0000  
#1st Qu.:1.0000   1st Qu.:0.0000  
#Median :1.0000   Median :1.0000  
#Mean   :0.9718   Mean   :0.5311  
#3rd Qu.:1.0000   3rd Qu.:1.0000  
#Max.   :1.0000   Max.   :1.0000  
#comten         ceoten      
#Min.   : 2.0   Min.   : 0.000  
#1st Qu.:12.0   1st Qu.: 3.000  
#Median :23.0   Median : 6.000  
#Mean   :22.5   Mean   : 7.955  
#3rd Qu.:33.0   3rd Qu.:11.000  
#Max.   :58.0   Max.   :37.000  
#sales          profits      
#Min.   :   29   Min.   :-463.0  
#1st Qu.:  561   1st Qu.:  34.0  
#Median : 1400   Median :  63.0  
#Mean   : 3529   Mean   : 207.8  
#3rd Qu.: 3500   3rd Qu.: 208.0  
#Max.   :51300   Max.   :2700.0  
#mktval     
#Min.   :  387  
#1st Qu.:  644  
#Median : 1200  
#Mean   : 3600  
#3rd Qu.: 3500  
#Max.   :45400 

model2 <- lm(log(salary)~log(mktval)+profits+ceoten, data = context2)
summary(model2)
#> summary(model2)

#Call:
#  lm(formula = log(salary) ~ log(mktval) + profits + ceoten, data = context2)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-2.63382 -0.34660  0.00627  0.35059  1.96220 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 4.7095052  0.3954502  11.909  < 2e-16 ***
#  log(mktval) 0.2386220  0.0559166   4.267 3.25e-05 ***
#  profits     0.0000793  0.0001566   0.506   0.6132    
#ceoten      0.0114646  0.0055816   2.054   0.0415 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.5289 on 173 degrees of freedom
#Multiple R-squared:  0.2514,	Adjusted R-squared:  0.2384 
#F-statistic: 19.36 on 3 and 173 DF,  p-value: 7.141e-11

model3 <- lm(log(salary)~log(mktval)+profits+ceoten+log(sales), data = context2)
summary(model3)
#> summary(model3)

#Call:
#  lm(formula = log(salary) ~ log(mktval) + profits + ceoten + log(sales), 
#     data = context2)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-2.48792 -0.29369  0.00827  0.29951  1.85524 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 4.558e+00  3.803e-01  11.986  < 2e-16 ***
#  log(mktval) 1.018e-01  6.303e-02   1.614   0.1083    
#profits     2.905e-05  1.503e-04   0.193   0.8470    
#ceoten      1.168e-02  5.342e-03   2.187   0.0301 *  
#  log(sales)  1.622e-01  3.948e-02   4.109 6.14e-05 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.5062 on 172 degrees of freedom
#Multiple R-squared:  0.3183,	Adjusted R-squared:  0.3024 
#F-statistic: 20.08 on 4 and 172 DF,  p-value: 1.387e-13

#j. profits can be negative, so we can not use natural logs on it.
#k. 1% increase of market value is associated with 0.24% increase in salary.
#l. 1% increase of market value is associated with 0.1% increase in salary.
#m. in modle2 the t-stat on market value is 4.267(significant at the 0.1% level) 
#    whereas in model3 the t-stat is 1.614(insignificant at the 10% level) 
#    including sales eliminates the effect of market value on CEO salary.
#    because of omitted variable bias, market value was a proxy-variable entirely 
#    capturing the effect of sales on  CEO salary.
#n. the coefficient of profits is not significant. 
#o. 1% increase in sales is assiciated with 0.16% increase in salary controlling for profits, CEO tenure, and market value of the firm.

#Question 3
context3 <- fread('hprice1.csv')
summary(context3)
#> summary(context3)
#price           assess     
#Min.   :111.0   Min.   :198.7  
#1st Qu.:230.0   1st Qu.:253.9  
#Median :265.5   Median :290.2  
#Mean   :293.5   Mean   :315.7  
#3rd Qu.:326.2   3rd Qu.:352.1  
#Max.   :725.0   Max.   :708.6  
#bdrms          lotsize     
#Min.   :2.000   Min.   : 1000  
#1st Qu.:3.000   1st Qu.: 5733  
#Median :3.000   Median : 6430  
#Mean   :3.568   Mean   : 9020  
#3rd Qu.:4.000   3rd Qu.: 8583  
#Max.   :7.000   Max.   :92681  
#sqrft         colonial     
#Min.   :1171   Min.   :0.0000  
#1st Qu.:1660   1st Qu.:0.0000  
#Median :1845   Median :1.0000  
#Mean   :2014   Mean   :0.6932  
#3rd Qu.:2227   3rd Qu.:1.0000  
#Max.   :3880   Max.   :1.0000

model4 <- lm(price~ bdrms+log(lotsize)+log(sqrft)+colonial, data = context3)
model5 <- lm(log(price)~ bdrms+log(lotsize)+log(sqrft)+colonial, data = context3)
summary(model4)
#> summary(model4)

#Call:
#  lm(formula = price ~ bdrms + log(lotsize) + log(sqrft) + colonial, 
#     data = context3)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-109.603  -38.258   -4.325   22.984  220.766 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -2030.455    210.967  -9.625 3.68e-15 ***
#  bdrms           18.572      9.308   1.995   0.0493 *  
#  log(lotsize)    61.446     12.372   4.966 3.60e-06 ***
#  log(sqrft)     225.508     30.072   7.499 6.41e-11 ***
#  colonial         4.134     14.509   0.285   0.7764    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 59.66 on 83 degrees of freedom
#Multiple R-squared:  0.6781,	Adjusted R-squared:  0.6626 
#F-statistic: 43.71 on 4 and 83 DF,  p-value: < 2.2e-16
summary(model5)
#> summary(model5)

#Call:
#  lm(formula = log(price) ~ bdrms + log(lotsize) + log(sqrft) + 
#       colonial, data = context3)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.69479 -0.09750 -0.01619  0.09151  0.70228 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -1.34959    0.65104  -2.073   0.0413 *  
#  bdrms         0.02683    0.02872   0.934   0.3530    
#log(lotsize)  0.16782    0.03818   4.395 3.25e-05 ***
#  log(sqrft)    0.70719    0.09280   7.620 3.69e-11 ***
#  colonial      0.05380    0.04477   1.202   0.2330    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.1841 on 83 degrees of freedom
#Multiple R-squared:  0.6491,	Adjusted R-squared:  0.6322 
#F-statistic: 38.38 on 4 and 83 DF,  p-value: < 2.2e-16

#p. every 1% increase in lotsize is associated with 0.61 units increase which is $610 increase in price.
#q. every 1% increase in lotsize is associated with 0.168% increase in price.
#r. (t) having a colonial hourse adds $4133.61 to your home value on average controlling for...
#r. colonial style is assiciated with 4.134 units($4134) increase in price.
#s. model 4 is better. The adjusted R-squared of model 4 is higher. 
#   (t) R square of model 4 is higher
#t. bedroom in crease 1 unit will increase price by 18.572k.
#   square-footage increaase by 10% will increase price by 22.55k.
#   new_price - 300k = 18.572 + 22.55  = 41.12 = 41.12k
#   expenditure = 50k - 20k = 30k
#   41.12k - 30k = 11.12k, the house value increased by 11.12k, so you should pursue the expansion.


#Question 4
context4 <- fread('JTRAIN2.csv')
summary(context4)
#> summary(context4)
#train             age             educ          black             hisp        
#Min.   :0.0000   Min.   :17.00   Min.   : 3.0   Min.   :0.0000   Min.   :0.00000  
#1st Qu.:0.0000   1st Qu.:20.00   1st Qu.: 9.0   1st Qu.:1.0000   1st Qu.:0.00000  
#Median :0.0000   Median :24.00   Median :10.0   Median :1.0000   Median :0.00000  
#Mean   :0.4157   Mean   :25.37   Mean   :10.2   Mean   :0.8337   Mean   :0.08764  
#3rd Qu.:1.0000   3rd Qu.:28.00   3rd Qu.:11.0   3rd Qu.:1.0000   3rd Qu.:0.00000  
#Max.   :1.0000   Max.   :55.00   Max.   :16.0   Max.   :1.0000   Max.   :1.00000  
#married          nodegree        mosinex           re74              re75       
#Min.   :0.0000   Min.   :0.000   Min.   : 5.00   Min.   : 0.0000   Min.   : 0.000  
#1st Qu.:0.0000   1st Qu.:1.000   1st Qu.:14.00   1st Qu.: 0.0000   1st Qu.: 0.000  
#Median :0.0000   Median :1.000   Median :21.00   Median : 0.0000   Median : 0.000  
#Mean   :0.1685   Mean   :0.782   Mean   :18.12   Mean   : 2.1023   Mean   : 1.377  
#3rd Qu.:0.0000   3rd Qu.:1.000   3rd Qu.:23.00   3rd Qu.: 0.8244   3rd Qu.: 1.221  
#Max.   :1.0000   Max.   :1.000   Max.   :24.00   Max.   :39.5707   Max.   :25.142  
#re78            unem74           unem75           unem78      
#Min.   : 0.000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
#1st Qu.: 0.000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
#Median : 3.702   Median :1.0000   Median :1.0000   Median :0.0000  
#Mean   : 5.301   Mean   :0.7326   Mean   :0.6494   Mean   :0.3079  
#3rd Qu.: 8.125   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000  
#Max.   :60.308   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000 

model6 <- lm(re78~re75+train+educ+black, data = context4)
summary(model6)
#> summary(model6)

#Call:
#  lm(formula = re78 ~ re75 + train + educ + black, data = context4)

#Residuals:
#  Min     1Q Median     3Q    Max 
#-9.120 -4.377 -1.756  3.353 54.058 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)  1.97686    1.89028   1.046   0.2962   
#re75         0.14697    0.09811   1.498   0.1349   
#train        1.68422    0.62700   2.686   0.0075 **
#  educ         0.41026    0.17267   2.376   0.0179 * 
#  black       -2.11277    0.82941  -2.547   0.0112 * 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 6.496 on 440 degrees of freedom
#Multiple R-squared:  0.04917,	Adjusted R-squared:  0.04053 
#F-statistic: 5.688 on 4 and 440 DF,  p-value: 0.00018

#u. every $1000 increase in real earning in 1975 is associated with $1470 increase in real earning in 1978.
#v. every 1 unit increase in job training is assciated with $1684 increase in real earns. The coefficient is significant at 1% level.
#v.(t) the value of job training is an extra $1684 increase in real earnings.
#w. every black people is assicated with $2112 decrease in real earns.
#w. (t) being black has an associated disadvantage of -$2112.77 controlling for education, job training, and real earnings in 75.