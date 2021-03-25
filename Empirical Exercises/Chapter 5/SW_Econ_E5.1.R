##### SW_Econ_E5.1

#####
# a. # i.
#####

# import data 
library(readxl)
Earnings_and_Height <- read_xlsx("TA Sessions/Empirical Exercises/Chapter 5/Earnings_and_Height/Earnings_and_Height.xlsx")

E51a_model <- lm(earnings ~ height, data = Earnings_and_Height)

summary(E51a_model)

#####
# a. # ii.
#####

E51a <- function(x, y){
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
  Rsqure <- ESS/TSS
  
  # standard error of the regression
  SER <- sqrt(SSR/(n-2))
  
  # standard error of coefficient
  se_b1hat <- sqrt(SER^2/sum((x - xbar)^2))
  se_b0hat <- sqrt(SER^2* (1/n + xbar^2/sum((x - xbar)^2)))
  
  # 95% CI for b1hat
  lower_b1hat <- round(b1hat - qnorm(0.975, mean = 0, sd = 1)*se_b1hat, digit = 4)
  upper_b1hat <- round(b1hat + qnorm(0.975, mean = 0, sd = 1)*se_b1hat, digit = 4)
  CI_b1hat <- paste(lower_b1hat, "-"  ,upper_b1hat)
  
  # 95% CI for b0hat
  lower_b0hat <- round(b0hat - qnorm(0.975, mean = 0, sd = 1)*se_b0hat, digit = 4)
  upper_b0hat <- round(b0hat + qnorm(0.975, mean = 0, sd = 1)*se_b0hat, digit = 4)
  CI_b0hat <- paste(lower_b0hat, "-"  ,upper_b0hat)  
  
  # coefficient
  coef <- matrix(c(b0hat, se_b0hat, CI_b0hat, b1hat, se_b1hat, CI_b1hat), ncol = 3, byrow = TRUE)
  rownames(coef) <- c("Intercept", "Slope")
  colnames(coef) <- c("Estimate", "Standard Error", "95% Confidence Interval")
  
  result <- list(coef, Rsqure)
  names(result) <- c("Coefficients", "R-squared")
  
  result
} 

E51a(Earnings_and_Height$height, Earnings_and_Height$earnings)


#####
# b. # i.
#####

# data: woman
Earnings_and_Height_women <- Earnings_and_Height[Earnings_and_Height$sex == 0,]

E51b_model <- lm(earnings ~ height, data = Earnings_and_Height_women)

summary(E51b_model)

#####
# b. # ii.
#####

E51a(Earnings_and_Height_women$height, Earnings_and_Height_women$earnings)


#####
# c. # i.
#####

# data: woman
Earnings_and_Height_men <- Earnings_and_Height[Earnings_and_Height$sex == 1,]

E51c_model <- lm(earnings ~ height, data = Earnings_and_Height_men)

summary(E51c_model)

#####
# c. # ii.
#####

E51a(Earnings_and_Height_men$height, Earnings_and_Height_men$earnings)

#####
# d. 
#####

E51d <- function(x1, y1, x2, y2){
  # numbers of sample
  n1 <- length(y1); n2 <- length(y2)
  
  # sample mean
  x1bar <- mean(x1); x2bar <- mean(x2)
  y1bar <- mean(y1); y2bar <- mean(y2)
  
  # OLS coefficient
  b1hat1 <- cov(x1, y1)/var(x1); b1hat2 <- cov(x2, y2)/var(x2)
  b0hat1 <- y1bar - b1hat1*x1bar; b0hat2 <- y2bar - b1hat2*x2bar
  
  y1hat1 <- b0hat1 + b1hat1*x1; y2hat2 <- b0hat2 + b1hat2*x2
  
  # explained sum of squares (ESS)
  ESS1 <- sum((y1hat1 - y1bar)^2); ESS2 <- sum((y2hat2 - y2bar)^2)
  # total sum of squares (TSS)
  TSS1 <- sum((y1 - y1bar)^2); TSS2 <- sum((y2 - y2bar)^2)
  # sum of squared residuals (SSR)
  SSR1 <- sum((y1 - y1hat1)^2); SSR2 <- sum((y2 - y2hat2)^2)
  
  # standard error of the regression
  SER1 <- sqrt(SSR1/(n1-2)); SER2 <- sqrt(SSR2/(n2-2))
  
  # standard error of coefficient
  se_b1hat1 <- sqrt(SER1^2/sum((x1 - x1bar)^2))
  se_b1hat2 <- sqrt(SER2^2/sum((x2 - x2bar)^2))
  
  est <- b1hat1 - b1hat2
  se <- sqrt(se_b1hat1^2 + se_b1hat2^2)
  
  # 95% CI for difference
  lower <- round(est - qnorm(0.975, mean = 0, sd = 1)*se, digit = 4)
  upper <- round(est + qnorm(0.975, mean = 0, sd = 1)*se, digit = 4)
  CI <- paste(lower, "-", upper)  
  
  # output table
  Table <- data.frame(est, se, CI)
  colnames(Table) <- c("Estimate", "Standard Error", "95% Confidence Interval")
  
  Table 
  
}

E51d(Earnings_and_Height_men$height, Earnings_and_Height_men$earnings, Earnings_and_Height_women$height, Earnings_and_Height_women$earnings)

#####
# e. 
#####

E51e <- function(x, y){
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
  lower_b1hat <- round(b1hat - qnorm(0.975, mean = 0, sd = 1)*se_b1hat, digit = 4)
  upper_b1hat <- round(b1hat + qnorm(0.975, mean = 0, sd = 1)*se_b1hat, digit = 4)
  CI_b1hat <- paste(lower_b1hat, "-"  ,upper_b1hat)
  
  Table <- matrix(c(b1hat, se_b1hat, CI_b1hat), nrow = 1)
  colnames(Table) <- c("Estimate", "Standard Error", "95% Confidence Interval")
  
  Table
} 

E51e_output <- matrix(nrow = 15, ncol = 3)

rownames(E51e_output) <- c(1:15)
colnames(E51e_output) <- c("Estimate", "Standard Error", "95% Confidence Interval")

for (i in 1:15){
  data <- Earnings_and_Height[Earnings_and_Height$occupation == i,]
  x <- data$height
  y <- data$earnings
  E51e_output[i,] <- E51e(x, y)
}

E51e_output











