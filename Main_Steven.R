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
