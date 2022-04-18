library(tidyverse)
library(MASS)
# devtools::install_github("cran/AER")
library(AER)

data("Boston") # load data



# Question 1 --------------------------------------------------------------
model <- lm(data=Boston,medv~lstat+crim+age) # writing model

# function to calculate t statistics for coefficients with p-values
t_test <- function(model, cef_no, value){
  coef <- coef(summary(model))
  tstat <- (coef[cef_no,1]-value)/coef[cef_no,2]
  list(pvalue=2 * pt(abs(tstat), model$df.residual, lower.tail = FALSE),
       tstat=tstat)
}


# For intercept
tstats1 <- t_test(model,1,0)
tstats1["tstat"] # value of t-statistics
tstats1["pvalue"] < 0.01

# For lstat
tstats2 <- t_test(model,2,0)
tstats2["tstat"] # value of t-statistics
tstats2["pvalue"] < 0.01

# For crim
tstats3 <- t_test(model,3,0)
tstats3["tstat"] # value of t-statistics
tstats3["pvalue"] < 0.01

# For age
tstats4 <- t_test(model,4,0)
tstats4["tstat"] # value of t-statistics
tstats4["pvalue"] < 0.01


stargazer::stargazer(model, type="text")



# Question 2 --------------------------------------------------------------

# coefficient intervals
confint(model,level=0.99)



# Question 3 --------------------------------------------------------------

lmtest::coeftest(model, vcov. = vcovHC)

pvals <- lmtest::coeftest(model, vcov. = vcovHC)[, 4] 
pvals < 0.01 # logical argument


# Question 4 --------------------------------------------------------------


modelQ4 <- lm(data = Boston,medv ~ lstat + I(crim + age))
stargazer::stargazer(modelQ4,type="text")

restricted_ssr <- sum(modelQ4$residuals^2)

unrestricted_model <- lm(data = Boston,medv ~ lstat + crim + age)
stargazer::stargazer(unrestricted_model,type="text")

unrestricted_ssr <- sum(unrestricted_model$residuals^2)
unrestricted_ssr


# Question 5 --------------------------------------------------------------

f_statistics <- ((restricted_ssr-unrestricted_ssr)/1)/(unrestricted_ssr/(nrow(Boston)-3-1))

p_value5 <- 1 - pf(f_statistics, df1 = 1, df2 = nrow(Boston)-3-1)

p_value5 < 0.01

linearHypothesis(unrestricted_model, "age = crim")



# Question 6 --------------------------------------------------------------

confidenceEllipse(model, which.coef = c("crim", "lstat"), levels = 0.99)

linearHypothesis(model, c("crim = 0", "lstat = 0"))
