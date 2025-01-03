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
data$REGION<-factor(data$REGION)
data$TYPE<-factor(data$TYPE)
data$BALCONY<-factor(data$BALCONY)

### Part 1: ----
#With a level of significance of 1%. 
#Hypotheses: Half of the houses have a Balcony
#Significance of 99% means
qnorm(p=0.005, mean=0, sd=1) #+-2.5758
#With R
pe=0.5
n=length(data$BALCONY)
result = binom.test(x=data$BALCONY=="Yes",n=n,p=pe,alternative="two.sided",conf.level=0.99)
result
#Null Hypotheses discarded. The proportion is not 0.5

#Calculations by hand
p0=count(~BALCONY,data=data, success="Yes")/n   #We get the sample proportion
print(paste0("Observed proportion: ",p0))
sd = sqrt(pe*(1-pe)/n)                          #Standar deviation using hypothesis value
sd
z_observed = (p0-pe)/sd
z_observed          #2.98 sd above the mean
p_value = 2*pnorm(q=z_observed, mean = 0, sd=1,FALSE)
p_value
#P_value observed is less that given with 99% significance. Reject null hypotheses
#Critical values:
z_crit = -qnorm(p=0.005,mean=0,sd=1)
LI = pe-z_crit*sd
LS = pe+z_crit*sd
print(paste0("Critical Interval from: [",LI, ", ",LS,"]."))
#Proportion Value outside confidence interval. Rejected. 
### Part 2: ----
hist(data$AREA,breaks=30)
#Significance level of 
#Size of the houses in the region is 75m^2
#Significance of 5% means
qt(p=0.025, df=n-1) #-1.96  
ev=75
t.test(x=data$AREA,alternative="two.sided",mu=ev,conf.level = 0.95)

### Part 3: ----

### Part 4: ----

### Part 5: ----

### Part 6: ----
