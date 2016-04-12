setwd('/Users/Anna/Documents/HS616/w7')
load( 'height_weight1.rdata')
load( 'height_weight2.rdata')
load( 'sampWXXXX.rdata')

#2
explore <- function( vec1, vec2, expected=0){
  #a
  len_vec1 <- length(vec1)
  len_vec2 <- length(vec2)
  print(sprintf("The length of vec1 is %s, the length of vec2 is %s",len_vec1,len_vec2))
  if(len_vec1==len_vec2){
    #b
    p_vec1 = shapiro.test(vec1)[2]
    p_vec2 = shapiro.test(vec2)[2]
    if(p_vec1 <= 0.05 & p_vec2 <= 0.05){
      print(sprintf("The p-value of vec1 is %s, vec1 is not normally distributated.",p_vec1))
      print(sprintf("the p-value of vec2 is %s, vec2 is not normally distributated.",p_vec2))
      check_var_p = ansari.test(vec1-median(vec1,na.rm=T),vec2-median(vec2,na.rm=T))[2]
    }else if(p_vec1 <= 0.05 & p_vec2 > 0.05){
      print(sprintf("The p-value of vec1 is %s, vec1 is not normally distributated.",p_vec1))
      print(sprintf("the p-value of vec2 is %s, vec2 is normally distributated.",p_vec2))
      check_var_p = ansari.test(vec1-median(vec1,na.rm=T),vec2-median(vec2,na.rm=T))[2]
    }else if(p_vec1 > 0.05 & p_vec2 <= 0.05){
      print(sprintf("The p-value of vec1 is %s, vec1 is normally distributated.",p_vec1))
      print(sprintf("the p-value of vec2 is %s, vec2 is not normally distributated.",p_vec2))
      check_var_p = ansari.test(vec1-median(vec1,na.rm=T),vec2-median(vec2,na.rm=T))[2]
    }else{
      print(sprintf("The p-value of vec1 is %s, vec1 is normally distributated.",p_vec1))
      print(sprintf("the p-value of vec2 is %s, vec2 is normally distributated.",p_vec2))
      check_var_p = var.test(vec1,vec2)[3]
    }
    #c
    if(check_var_p <= 0.05){
      print(sprintf("vec1 and vec2 do not have the same variance with p-value %s", check_var_p))
      p_t_vec1_vec2 = t.test(vec1,vec2, var.equal = F)[3]
      t_2test = t.test(vec1,vec2, var.equal = F)
    }else{
      print(sprintf("vec1 and vec2 have the same variance with p-value %s", check_var_p))
      p_t_vec1_vec2 = t.test(vec1,vec2, var.equal = T)[3]
      t_2test = t.test(vec1,vec2, var.equal = T)
    }
    #d
    check_cor <- cor(vec1,vec2)
    print(sprintf("The correlation between vec1 and vec2 is %s",check_cor))
    #e,f
    p_t_vec1 = t.test(vec1,mu=expected)[3]
    p_t_vec2 = t.test(vec2,mu=expected)[3]
    if(p_t_vec1 <= 0.05){
      print(t.test(vec1,mu=expected))
      print(sprintf("The p-value is %s, the mean of vec1 is different from expected", p_t_vec1))
    }else{
      print(t.test(vec1,mu=expected))
      print(sprintf("The p-value is %s, the mean of vec1 is not different from expected", p_t_vec1))
    }
    if(p_t_vec2 <= 0.05){
      print(t.test(vec2,mu=expected))
      print(sprintf("The p-value is %s, the mean of vec2 is different from expected", p_t_vec2))
    }else{
      print(t.test(vec2,mu=expected))
      print(sprintf("The p-value is %s, the mean of vec2 is not different from expected", p_t_vec2))
    }
    #g
    if(p_t_vec1_vec2 <= 0.05){
      print(t_2test)
      print(sprintf("the means of vec1 and vec2 are different with p-value %s",p_t_vec1_vec2))
    }else{
      print(t_2test)
      print(sprintf("the means of vec1 and vec2 are the same with p-value %s",p_t_vec1_vec2))
    }
    return
  }else{
    print("Error Message: vec1 and vec2 do not have equal length ")
    return
  }
}

#3
v1 <- rnorm(100)
v2 <- rnorm(1000)
explore(v1,v2,2)
#v3 <- rnorm(100,50,10)
#v4 <- rnorm(100)
#v5 <- rbinom(100,1,0.5)
#explore(v1,v3)
#explore(v1,v4)
#explore(v1,v5)

#4
explore(sampH2010_1,sampH2010_2, 1.76)

#5
explore(sampH1960_1, sampH1960_2, 1.76)

#6
explore(sampH2010_1,sampH1960_1, 1.76)
explore(sampH2010_2,sampH1960_2, 1.76)
#The null hypothesis is: the mean of sampH2010_1 and sampH1960_1 are the same.
#Both sampH2010_1 and sampH1960_1 are normally distributated with same variance
#and same expected mean.
#The null hypothesis is: the mean of sampH2010_2 and sampH1960_2 are the same.
#Both sampH2010_2 and sampH1960_2 are normally distributated with same variance
#and same expected mean.

#7
explore(sampW2010_1, sampW2010_2, 88.7)

#8
explore(sampW1960_1, sampW1960_2, 88.7)

#9
explore(sampW2010_1, sampW1960_1, 88.7)
explore(sampW2010_2, sampW1960_2, 88.7)
#The null hypothesis is: the mean of sampW2010_1 and sampW1960_1 are the same.
#the p-value for the 2 sample t-test is 0.056, meaning the null hypothesis is at
#the border line. The true mean for sampW2010_1 is between 83.13647 98.33703
#The true mean for sampW1960_1 is between 74.20216 87.90455. Meaning The average weight
#increased over time. 
#The null hypothesis is: the mean of sampW2010_2 and sampW1960_2 are the same.

#10
library(ggplot2)
library(reshape)
df_height <- data.frame(sampH2010_1,sampH2010_2,sampH1960_1, sampH1960_2)
long_df_height <- melt(df_height)
ggplot(long_df_height, aes(x = value, fill = variable, xmin = 1, xmax = 2.5)) + 
  geom_density(alpha=0.5)
#The height of these 4 vectors are normally distributated, with a mean of 1.76.

#11
df_weight <- data.frame(sampW2010_1, sampW2010_2,sampW1960_1, sampW1960_2)
long_df_weight <- melt(df_weight)
ggplot(long_df_weight, aes(x = value, fill = variable, xmin = 0, xmax = 170)) + 
  geom_density(alpha=0.5)
#The weight of sampW2010_1 is not normally distributated. The means of sampW2010_1 and sampW2010_2
#are greater than the means of sampW1960_1 and sampW1960_2. 

#12
library (GGally)
weight_data <- data.frame(sampWXXXX,df_weight)
ggpairs(weight_data)
fit1 <- lm(sampWXXXX~sampW2010_2)
summary(fit1)
#sampWXXXX = 2.205*sampW2010_2 ## the intercept -1.819e-13 is droped because it's close to 0.
#according to the formula, sampWXXXX and sampW2010_2 are positively correlated, the 
#correlation is 1.

#13
fit2 <- lm(sampWXXXX~sampW2010_2+sampH2010_2)
summary(fit2)
#sampWXXXX = (2.205e+00)sampW2010_2
#the intercept (-1.819e-13) is droped because it's close to 0.
#the slop of sampH2010_2 (-3.463e-15) is droped because it's close to 0.

#14
fit3 <- lm(sampWXXXX~sampW2010_2*sampH2010_2)
summary(fit3)
##fit3 :sampWXXXX = (2.205e+00)*sampW2010_2
#the intercept (5.002e-13) is droped because it's close to 0.
#the slop of sampH2010_2 (-3.959e-13) is droped because it's close to 0.
#the slop of sampW2010_2:sampH2010_2 (4.657e-15) is droped because it's close to 0.
#F-statistic: (variables explained by the module)/(variables not explained by the module)

fit4 <- lm(sampWXXXX~sampW2010_2+sampH2010_2+sampW2010_2:sampH2010_2) 
summary(fit4)
##fit4 :sampWXXXX = (2.205e+00)*sampW2010_2
#the intercept (5.002e-13) is droped because it's close to 0.
#the slop of sampH2010_2 (-3.959e-13) is droped because it's close to 0.
#the slop of sampW2010_2:sampH2010_2 (4.657e-15) is droped because it's close to 0.

#fit1:Adjusted R-squared is 1
#fit2:Adjusted R-squared is 1
#fit3:Adjusted R-squared is 1
#fit4:Adjusted R-squared is 1
#They are all perfectly fit into the module.

#15
plot(fit1)
#The Residuals are randomly distributated.
#The Residuals are followed the standerdized distruibution in Normal Q-Q plot.
plot(fit4)
#The Residuals are randomly distributated in fit4.
#The Residuals of fit4 are followed the standerdized distruibution in Normal Q-Q plot.

#16
permute_w <- sample(sampWXXXX, replace=F)
weight_data2 <- data.frame(permute_w,weight_data)
ggpairs(weight_data2)
fit5 <- lm(permute_w~sampW2010_2*sampH2010_2)
summary(fit5)
#permute_w = (0.1534)*sampW2010_2 - (31.5654)*sampH2010_2 -(0.3548)*(sampW2010_2:sampH2010_2) + 289.1858
#adjusted R2 is -0.05497
#Compare to fit1 and fit4, the module of permute_w is a poor module with
#adjusted R2 is -0.05497. 

#17
new_w <- 0.4536*sampWXXXX - .3*sampW2010_2*sampH2010_2 + .6*permute_w
cor(new_w,permute_w)
cor(new_w,sampWXXXX)
cor(new_w,sampW2010_2*sampH2010_2)
cor(new_w,df_weight)
# the slope of permute_w is 0.6, meaning positively correlated with new_w with correlation of 0.9
# the slope of sampWXXXX is 0.4536,and slop of*sampW2010_2*sampH2010_2 is -0.3
# These 2 fectors work together to affect new_w with positive correlations of 0.21 and 0.06
# new_w is lightly correlated with other 4 vectors because of '0.4536*sampWXXXX - .3*sampW2010_2*sampH2010_2"

#18
fit6 <- lm(new_w~permute_w*sampW2010_2*sampH2010_2)
summary(fit6)
#adjusted R2: 1
#formula: new_w = 0.6*permute_w +1*sampW2010_2 -0.3*sampW2010_2*sampH2010_2 

#19
fit7 <- lm(new_w~permute_w*sampW2010_2*sampH2010_2*sampWXXXX )
summary(fit7)
#adjusted R2:1 
#formula: new_w = 0.6*permute_w +1*sampW2010_2 -0.3*sampW2010_2*sampH2010_2 


