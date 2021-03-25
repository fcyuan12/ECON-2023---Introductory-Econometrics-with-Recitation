##### SW_Econ_E5.3

#####
# a. 
#####

# import data 
library(readxl)
Birthweight_Smoking <- read_xlsx("TA Sessions/Empirical Exercises/Chapter 5/Birthweight_Smoking/Birthweight_Smoking.xlsx")

E53a <- function(x){
  # i. sample mean
  mu <- mean(x)
  
  # ii. standard error = sample standard deviation (s) / sqrt(n)
  se <- sd(x)/sqrt(length(x))
  
  # test
  test <- t.test(x, 
                 alternative = c("two.sided"),
                 mu = 0, # H0
                 conf.level = 0.95) # alpha = 0.05
  
  # iii. 95% confidence interval
  lower <- round(test$conf.int[1], digit = 4)
  upper <- round(test$conf.int[2], digit = 4)
  CI <- paste(lower, "-"  ,upper)
  
  Table <- data.frame(mu, se, CI)
  colnames(Table) <- c("Mean", "Standard Error", "95% Confidence Interval")
  
  Table
  
}

# i 
E53a(Birthweight_Smoking$birthweight)

# ii. # iii.
tapply(Birthweight_Smoking$birthweight, Birthweight_Smoking$smoker, E53a)


#####
# b. 
#####

# data: non-smoker
Birthweight_Smoking0 <- Birthweight_Smoking[Birthweight_Smoking$smoker == 0,]

# data: smoker
Birthweight_Smoking1 <- Birthweight_Smoking[Birthweight_Smoking$smoker == 1,]

E53b <- function(x1, x2){
  # mean
  mu1 <- mean(x1); mu2 <- mean(x2)
  
  # s
  SD1 <- sd(x1); SD2 <- sd(x2)
  
  # n
  n1 <- length(x1); n2 <- length(x2)
  
  # i. # difference in mean
  mu <- mu2 - mu1
  
  # ii. # difference in standard error
  se <- sqrt(SD1^2/n1 + SD2^2/n2)
  
  # iii. # 95% confidence interval
  lower <- round(mu - qnorm(0.975, mean = 0, sd = 1)*se, digit = 4)
  upper <- round(mu + qnorm(0.975, mean = 0, sd = 1)*se, digit = 4)
  CI <- paste(lower, "-"  ,upper)
  
  Table <- data.frame(mu, se, CI)
  colnames(Table) <- c("Mean", "Standard Error", "95% Confidence Interval")
  
  Table  
  
}

E53b(Birthweight_Smoking0$birthweight, Birthweight_Smoking1$birthweight)

#####
# c. 
#####

E53c <- lm(birthweight ~ smoker, data = Birthweight_Smoking)

summary(E53c)






