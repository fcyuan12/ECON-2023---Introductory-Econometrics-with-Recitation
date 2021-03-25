##### SW_Econ_E5.2

#####
# a. 
#####

# import data 
library(readxl)
Growth <- read_xlsx("TA Sessions/Empirical Exercises/Chapter 5/Growth/Growth.xlsx")

Growth_n <- Growth[Growth$country_name != "Malta",]

E52a_model <- lm(growth ~ tradeshare, data = Growth_n)

summary(E52a_model)

#####
# c. 
#####

E52c <- function(x, y){
  # numbers of sample
  n <- length(y)
  
  # sample mean
  xbar <- mean(x)
  ybar <- mean(y)
  
  # OLS coefficient
  b1hat <- cov(x,y)/var(x)
  b0hat <- ybar - b1hat*xbar
  
  yhat <- b0hat + b1hat*x
  
  # explained sum of squares (ESS)
  ESS <- sum((yhat - ybar)^2)
  # total sum of squares (TSS)
  TSS <- sum((y - ybar)^2)
  # sum of squared residuals (SSR)
  SSR <- sum((y - yhat)^2)
  
  # coefficient of determination
  #Rsqure <- ESS/TSS
  
  # standard error of the regression
  SER <- sqrt(SSR/(n-2))
  
  # standard error of coefficient
  se_b1hat <- sqrt(SER^2/sum((x - xbar)^2))
  
  # 95% CI for b1hat
  lower_b1hat <- round(b1hat - qnorm(0.95, mean = 0, sd = 1)*se_b1hat, digit = 4)
  upper_b1hat <- round(b1hat + qnorm(0.95, mean = 0, sd = 1)*se_b1hat, digit = 4)
  CI_b1hat <- paste(lower_b1hat, "-"  ,upper_b1hat)
  
  Table <- matrix(c(b1hat, se_b1hat, CI_b1hat), nrow = 1)
  colnames(Table) <- c("Estimate", "Standard Error", "90% Confidence Interval")
  
  Table
} 

E52c(Growth_n$tradeshare, Growth_n$growth)










