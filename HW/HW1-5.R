library(tidyverse)
# Lesson 1 ----------------------------------------------------------------

#create a dataset
#Name columns as x1=Gender, x2=Height, x3=Weight
#Create  a new column BMI which denotes Body mass Index with the following formula: bmi= weight/height^2
#Suppose obese condition applies to male if BMI>28 or female IF BMI>30
#Filter observations with obese condition and make a scatterplot ( Weight x Height)

Gender <- rbinom(100, size=1, p=0.5)
Gender <-  ifelse(Gender == 1,"Male","Female")
Height <- runif(n = 100, min = 40, max = 150)
Weight <- runif(n=100, min=40, max=100)

data <- data.frame(Gender, Height, Weight)

# Function to calculate BMI
bmi_calc <- function(height, weight){
  bmi = (weight/(height/100)^2)
}

data <- data %>% mutate(BMI = bmi_calc(height=Height, weight = Weight)) # Added BMI column

data$BMI %>% min() # minimum bmi
data$BMI %>% max() # maximum bmi


data %>% filter(BMI>30 & Gender=="Female") %>% 
  ggplot()+
  geom_point(aes(x=Weight, y=Height), col="steelblue")+
  ggtitle("Scatterplot showing Heights against Weights")+
  labs(subtitle="For a substet of Females with BMI>30")




# Lesson 2 ----------------------------------------------------------------



#a)Draw a sample of size=100 from a Standard Normal Distribution
#b)Perform a t-test "by hand" (without the R function)
#c)Perform the t-test using the function from R and compare the values
#d)Interpret the result of the t-test
#d) Repeat step a) 1000 times, storing the mean on a vector and plot the histogram


sample_norm <- rnorm(100)
samplemean <- mean(sample_norm)

SE_samplemean <- sqrt(samplemean * (1 - samplemean) / 100) #Standard error


# null hypothesis
mean_h0 <- 0.1

# compute the p-value
pvalue <- 2 * pnorm(- abs(SE_samplemean - mean_h0) / SE_samplemean) #pnorm gives the distribution function of Normal
pvalue

# compute a t-statistic for the sample mean
tstatistic <- (SE_samplemean - mean_h0) / SE_samplemean
tstatistic

(mean(sample_norm)-0.1)/sqrt(var(sample_norm)/100)


# simulation of ttest 1000 times
samplemean<-numeric(1000)
SE_samplemean<- numeric(1000)
pvalue <- numeric(1000)
tstatistic <- numeric(1000)


for (i in 1:1000) {
  
  sample_norm <- rnorm(100)
  samplemean[i] <- mean(sample_norm)
  SE_samplemean[i] <- sqrt(samplemean * (1 - samplemean) / 100) #Standard error
  # null hypothesis
  mean_h0 <- 0.1
  
  # compute the p-value
  pvalue[i] <- 2 * pnorm(- abs(SE_samplemean - mean_h0) / SE_samplemean) #pnorm gives the distribution function of Normal
  tstatistic[i] <- (SE_samplemean - mean_h0) / SE_samplemean

}

data.frame(tstatistic,pvalue) %>% unique()

# Lesson 3 ----------------------------------------------------------------


#We can check the unbiasedness and consistency of OLS estimators via MONTE CARLO simulations

#Routine:
#1-Consider the model Y=b0+b1*x+u
#2-Define a value to the parameters b0 and b1 creating these objects
b0 <- 0.5
b1 <- 1.7
#3-Generate the variable x, drawing a sample (size=50) from a normal distribution
X <- rnorm(50)
#4-Generate the variable u, drawing from the standard normal distribution
u <- rnorm(50)
#5-Create the variable Y=b0+b1*x+u
Y = b0 + b1*X+u
#6-Estimate the parameters via OLS.
mod<-lm(Y~X)
#7-Get the coefficients. The values might be close to the parameters.
coefficients(mod)
#8-Check how close they are, performing a t-test and interpreting it.Tip: use pnorm()
t.test(Y~X) # nor running

pred = predict(mod)
plot(Y~X)
abline(mod, col = "red")
lines(sort(X), pred)


#Simulations:
#Repeat the previous routine (steps 3 to 7) 1000 times. Tip: use "for" 
#For every estimation, store the coefficients b0 and b1 in empty vectors previously created.
#Get the mean of each vector (b0 and b1). 
#Are they close to the true parameters? Are the OLS estimators biased?
#Plot the histogram of each of them

# initialize the employment vector

#REPEAT ALL THE SIMULATIONS, BUT WITH SAMPLE SIZE EQUAL TO 200, THEN EQUAL TO 1000.
#Plot the 3 histograms (sample size =50, 200 and 1000) of b0 side by side to compare.
#Do the same with the 3 histograms of b1.
#TIP: USE THE SAME RANGE ON X-AXIS (argument xlim= ) TO A BETTER COMPARISON
#Are the OLS estimators consistent?


Date <- seq(as.Date("1951/1/1"), as.Date("2000/1/1"), "years")

# initialize the employment vector
X <- c(50, rep(NA, length(Date)-1))

for (i in 2:length(Date)) {
  
  X[i] <- -50 + 0.98 * X[i-1] + rnorm(n = 1, sd = 200)
  
}

plot(x = Date, 
     y = X, 
     type = "l", 
     col = "steelblue")

# 200



Date <- seq(as.Date("1951/1/1"), as.Date("2000/1/1"), "years")

# initialize the employment vector
X <- c(200, rep(NA, length(Date)-1))

for (i in 2:length(Date)) {
  
  X[i] <- -200 + 0.98 * X[i-1] + rnorm(n = 1, sd = 200)
  
}

plot(x = Date, 
     y = X, 
     type = "l", 
     col = "steelblue")

# 1000

Date <- seq(as.Date("1951/1/1"), as.Date("2000/1/1"), "years")

# initialize the employment vector
X <- c(1000, rep(NA, length(Date)-1))

for (i in 2:length(Date)) {
  
  X[i] <- -1000 + 0.98 * X[i-1] + rnorm(n = 1, sd = 200)
  
}

plot(x = Date, 
     y = X, 
     type = "l", 
     col = "steelblue")


# Lesson 4 ----------------------------------------------------------------


# HANDS ON! MONTE CARLO SIMULATIONS ---------------------------------------
# Setting a confidence level of 95%, in about 5% of cases we will wrongly reject the null.
# However, the fraction of rejections will be greater if we do not use robust
# standard errors when errors are heteroskedastic!
#Test this, via Monte Carlo Simulations!

#Loop Routine:
#1 - Consider the simple linear regression model Y=B0+B1*X+u 
#2-Generate X, u and Y and violating the homoskedasticity assumption. Sample size=1000
#3-Estimate the model
#4-Perform t-test for B1 using non-robust and robust standard errors,  
# checking to false rejection. 
#Tip1:Use the function linearHypothesis()
#Tip1: store the results in logic vectors
#Compare the fraction of false rejections in each case
#What are the implications of not using robust standard errors in this case?

library(lmtest)
library(sandwich)
library(car)

x=rnorm(n=100, mean = 5, sd=1)
u=rnorm(100)
b0 = 2
b1 = 1
Y = b0+b1*x+u
reg <-lm(Y~x)
summary(reg)

# t test using non robust SE
seb <-  summary(reg)$coefficients[4]
t_test=(summary(reg)$coefficients[2]-0)/seb

(t_by_hand=((summary(reg)$coefficients[2]-0)/seb)==summary(reg)$coefficients[,3][2])


# with robust SE

vcov <- vcovHC(reg, type = "HC1") 
robust_se <- sqrt(diag(vcov));robust_se 

t_test_robust = (summary(reg)$coefficients[2]-0)/robust_se[2]
t_test_robust


linearHypothesis(reg, hypothesis.matrix = "x = 0.5")



#SAMPLE SIZE =1000
vector_b0 <- numeric(1000)
vector_b1 <- numeric(1000)
seb <- numeric(1000)
t_test <- numeric(1000)
t_test_robust <- numeric(1000)
lhpv<- numeric(1000)
lhdf<-numeric(1000)

for (i in 1:1000) {
  x=rnorm(1000, mean = 10, sd=1) 
  u=rnorm(1000) 
  Y=b0+b1*x+u 
  model1 <- lm(Y~x)
  vector_b0[i] <- model1$coefficients[1] #get the estimate of b0 and store
  vector_b1[i] <- model1$coefficients[2] #get the estimate of b1 and store
  seb[i] <-  summary(model1)$coefficients[4]
  t_test[i] =(summary(model1)$coefficients[2]-0)/seb
  t_test_robust[i] = (summary(model1)$coefficients[2]-0)/sqrt(diag(vcovHC(model1, type = "HC1")))[2]
  lhdf[i] <- linearHypothesis(model1, hypothesis.matrix = "x = 0.5")[,1][2]
  lhpv[i] <- linearHypothesis(model1, hypothesis.matrix = "x = 0.5")[,6][2]

}

seb
t_test
t_test_robust

linear_hyp_sim_results <- data.frame(lhdf, lhpv) %>% mutate(pval_reg = ifelse(lhpv>0.05, F,T))
linear_hyp_sim_results$pval_reg %>% table() # all are significant



