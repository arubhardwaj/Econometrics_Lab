# HANDS ON! ESTIMATING A MULTIPLE LINEAR REGRESSION MODEL ---------------------

#Load the dataset 'mtcars' (check the help for information about it)
data("mtcars")

#Check the descriptive statistics of the variables and the correlation
library(stargazer)
stargazer(mtcars,type="text")
#Estimate a multiple linear model to identify the variables that impact 
#the fuel consumption of a car
mod <- lm(mpg~., data=mtcars) # regression with all variables
stargazer(mod, type="text")
summary(mod) # can also use summary()
step(mod) %>% summary() # step wise version of all variable to know what variables are significant from models

### regression with selected impactful vars

mod1 <- lm(mpg~wt+cyl+gear,data=mtcars)
stargazer(mod1, type="text")
summary(mod1) # can also use summary()

#Test the assumption of heteroskedasticity

lmtest::bptest(mod1)### using BP test for heteroskedasticity
#' since the p-value is > 0.05 the null hypothesis of homoskedasticity cannot be rejected
#' Thus there are sufficient evidence that there is no heteroskedasticity in our this models

#Interpret the results of the model
#' the model shows sufficient evidence that weight and number of cylinders have 
#' impact on the milege per hour, as their p-value if significant
#' However, it determined the negative impact on mpg as coefficients are negative for both


#From the same model, include another variable and repeat the tests and interpretations

mod2 <- lm(mpg~wt+cyl+gear+vs,data=mtcars)
stargazer(mod2, type="text")
summary(mod2) # can also use summary()

#Test the assumption of heteroskedasticity

lmtest::bptest(mod2)
#' since the p-value is > 0.05 the null hypothesis of homoskedasticity cannot be rejected
#' Thus there are sufficient evidence that there is no heteroskedasticity in our this models



#What was the effect of including this new variable? Would you keep it in the model?
#' I included the shape of engine as it may have impact on the milege
#' however it seems not to have a good fit in the model considering it have very hig p-value
#' and also standard error and t-statisitcs do not recommend for its significance

#Create a dummy variable and include it in your model, commenting the result.

mtcars$am_dummy <- mtcars$am %>% as.factor() # dummy variable of am variable
mod3 <- lm(mpg~wt+cyl+gear+vs+am_dummy,data=mtcars)
stargazer(mod3, type="text")



#Generate random data for a new car and predict the values of it's fuel consumption
#based on your model

milege <- sample(x = 10:34, size  = 10000,replace = T)
weight <- sample(x = 1:6, size  = 10000,replace = T)
engine <- sample(c(1,2), size = 10000, prob = c(0.7,0.3), replace = TRUE) %>% as.factor()
df <- data.frame(milege,weight,engine)

mod_my<-lm(milege~weight+engine) 

lm(milege~weight+engine) %>% stargazer(type="text")

predict(mod_my,df) %>% head()
