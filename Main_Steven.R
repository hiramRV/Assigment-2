#################
## Main code for Assignment 2
## Integrates:
## Caroline
## Faezeh
## Pablo
## Steven
#################

library(openxlsx)
library(mosaic)
library(RColorBrewer)
library(DescTools)
library(e1071)
library(scales)

data <-read.xlsx("dataset01.xlsx")

### Pre-processing ----
str(data)
View(data)
data$REGION<-factor(data$REGION)
data$TYPE<-factor(data$TYPE)
data$BALCONY<-factor(data$BALCONY)

### Part 1: ----
#With a level of significance of 1%. 
#Hypotheses: Half of the houses have a Balcony
#Significance of 99% means
z_est=abs(qnorm(p=0.01, mean=0, sd=1)) #+-2.3263
p0=0.5
#Another alternative
#With R
ci_balcony <- prop.test(x=data$BALCONY=="Yes", n=n, conf.level = 0.99)
ci_balcony$conf.int
ci_balcony$p.value
#The proportion is not 0.5
#By hand
p_balc=count(~BALCONY,data=data, success="Yes")/n   #We get the sample proportion
print(paste0("Observed proportion: ",p0))
se = sqrt(p0*(1-p0)/n) 
z_observed = (p_balc-p0)/se
z_observed          #2.9837 se above the mean
p_value = 2*(1-pnorm(q=z_observed))
print(paste0("p-value with R of: ",ci_balcony$p.value)) 
print(paste0("p-value by hand of: ",p_value)) 


### Part 2: ----
hist(data$AREA,breaks=30)
n=length(data$AREA)
#We don't care about the distribution.
#Significance level of 
#Size of the houses in the region is 75m^2
#Significance of 5% means
qt(p=0.025, df=n-1) #-1.96  
mu_2=75
#Using R
test2=t.test(x=data$AREA,alternative="two.sided",mu=ev,conf.level = 0.95)
print(paste0("With a P-value of: ",round(test2$p.value,4),". We reject the null with a conf level of 95%"))

#By Hand Calculations
x_obs = mean(data$AREA)
print(paste0("Observed price: ",x_obs))
s = sqrt(sum((data$AREA - mu_2)^2/(n - 1))) 
se_2 = s/sqrt(n)
t_obs2 = (x_obs-mu_2)/se_2
print(paste0("Observed t-value: ",t_obs))  #2.839
p_value2 = 2*(1-pt(q=t_obs2,df=n-1))       #Calculated using t-test.
print(paste0("p-value with R of: ",test2$p.value)) 
print(paste0("p-value by hand of: ",p_value2)) 

### Part 3: ----
#Estimate the proportion of apartments
#With R
n=length(data$BALCONY)
ci_balcony_3 <- prop.test(x=data$TYPE=="Apartment", n=n, conf.level = 0.90)
ci_balcony_3$conf.int

#By Hand: We don't have the population sd so we use t-student
p_obs3=count(~TYPE,data=data, success="Apartment")/n   #We get the sample proportion
print(paste0("Observed proportion: ",p_obs3))
sp = sqrt(p_obs3*(1-p_obs3)/n)
#Confidence interval
alpha = 0.1
z_est3=abs(qnorm(p=0.05))  #alpha/2, around 1.6448
CI_HAND_lower = p_obs3-z_est3*sp
CI_HAND_upper = p_obs3+z_est3*sp
print(paste0("C.I. proportion of apartments with R: ",ci_balcony_3$conf.int[1], ", ",ci_balcony_3$conf.int[2]))
print(paste0("C.I. proportion of apartments by hand: ",CI_HAND_lower, ", ",CI_HAND_upper))

### Part 4: ----
#Estimate the expected number of rooms
#With R
n=length(data$ROOMS)
test4= t.test(data$ROOMS,conf.level = 0.95)

#By Hand
p_obs_4 = mean(data$ROOMS)
alpha = 0.05
t_est4= qt(0.975,n-1) #1.96
sx = sd(data$ROOMS)
CI_ROOMS_l4 = p_obs_4-t_est4*sx/sqrt(n)
CI_ROOMS_u4 = p_obs_4+t_est4*sx/sqrt(n)

print(paste0("C.I. number of rooms with R: ",test4$conf.int[1], ", ",test4$conf.int[2]))
print(paste0("C.I. number of by hand: ",CI_ROOMS_l4, ", ",CI_ROOMS_u4))

### Part 5: ----

### Part 6: ----
