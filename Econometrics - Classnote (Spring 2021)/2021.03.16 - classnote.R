library(readxl)
Earnings_and_Height <- read_xlsx("Earnings_and_Height/Earnings_and_Height.xlsx")

# list all variable names
names(Earnings_and_Height)

# a.
median(Earnings_and_Height$height)

# summary statistics
summary(Earnings_and_Height$height)

# b. # i.

# create empty variable
# "group"
group <- c()

# If “height <= 67”, then “group = 0.”
# If “height > 67”, then “group =1.” 
for (i in 1:length(Earnings_and_Height$height)){
  if (Earnings_and_Height$height[i] <= 67){
    # height <= 67
    group[i] <- c(0)
  } else{
    # height > 67
    group[i] <- c(1)
  }
}

# cbind: column bind
# combine columns
Earnings_and_Height <- cbind(Earnings_and_Height, group)

E42b <- function(x){
  # sample mean
  mu <- mean(x)
  
  # sample standard deviation (standard error)
  se <- sd(x)/sqrt(length(x))
  
  # test
  test <- t.test(x, 
                 alternative = c("two.sided"),
                 mu = 0, # H0
                 conf.level = 0.95) # alpha = 0.05
  
  # 95% confidence interval
  lower <- round(test$conf.int[1], digit = 4)
  upper <- round(test$conf.int[2], digit = 4)
  CI <- paste(lower, "-"  ,upper)
  
  Table <- data.frame(mu, se, CI)
  colnames(Table) <- c("Mean", "Standard Error", "95% Confidence Interval")
  
  Table
  
}

# use variables A with 
# group B
# to do function C
# tapply(A, B, C)
tapply(Earnings_and_Height$earnings, Earnings_and_Height$group, E42b)

# c
plot(x = Earnings_and_Height$height,
     y = Earnings_and_Height$earnings,
     pch = 16, # filled circle
     col = "black",
     xlim = c(45, 90),
     ylim = c(0, 85000),
     xlab = "height",
     ylab = "earnings",
     main = "E4.2 (c)")
# find correlation 
cor(Earnings_and_Height$height,
    Earnings_and_Height$earnings)

# d
# Earnings on Height
# Y on X
# lm(y ~ x)

# regression 
E42d <- lm(formula = earnings ~ height, data = Earnings_and_Height)

# i. estimated intercept, estimated slope
summary(E42d)


# E5.1
# a
E51a_model <- lm(earnings ~ height, data = Earnings_and_Height)

summary(E51a_model)

# iii
E51a <- function(x,y){
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

  # 95% CI: 
  # b1hat+-Z*SE(b1)
  
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

# if 0 is in CI, 
# then we don't reject H0
# if 0 isn't in CI,
# then we reject H0

# H0: p=0.5

