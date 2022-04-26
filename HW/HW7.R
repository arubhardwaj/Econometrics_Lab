
library(MASS)
library(AER)
data("Boston")
attach(Boston)
# simple linear model:


linear <- lm(medv~ptratio+tax+lstat+chas)
linear_log <- lm(medv~ptratio+tax+lstat+chas)
cubic <- lm(medv~ptratio+I(tax^3)+lstat+chas)
cubic_log <- lm(log(medv)~log(ptratio)+I(tax^3)+lstat+chas)

quadratic <- lm(medv~ptratio+I(tax^4)+lstat+chas)
quadratic_log <- lm(log(medv)~log(ptratio)+I(tax^4)+lstat+chas)

rob_se <- list(sqrt(diag(vcovHC(linear, type = "HC1"))),
               sqrt(diag(vcovHC(linear_log, type = "HC1"))),
               sqrt(diag(vcovHC(cubic, type = "HC1"))),
               sqrt(diag(vcovHC(cubic_log, type = "HC1"))),
               sqrt(diag(vcovHC(quadratic, type = "HC1"))),
               sqrt(diag(vcovHC(quadratic_log, type = "HC1"))))


stargazer::stargazer(linear, linear_log, cubic,cubic_log,quadratic,quadratic_log,
                     type = "text",
                     se = rob_se)

#' There are 6 models in 1,2, and 5 medv is dependent variable
#' also in 4th and 6th medv is taken at log scale.

#' the analysis in all forms of models suggest negative influence of ptratio on medv
#' plus lstat has also negative and strong impact on medv.
# However, chas has positibe coefficient value which is statistically significant
# throughout the R-squared is strong 


# Find the best model -----------------------------------------------------



rmse <- function(model){
  sqrt(mean(model$residuals^2))
  
}


rmse(linear) 
rmse(linear_log)
rmse(cubic) # small
rmse(cubic_log)
rmse(quadratic) # small
rmse(quadratic_log)
