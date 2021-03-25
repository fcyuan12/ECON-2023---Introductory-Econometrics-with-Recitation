##### SW_Econ_E3.1

#####
# a.
#####

# import data 
library(readxl)
CPS96_15 <- read_xlsx("TA Sessions/Empirical Exercises/Chapter 3/CPS96_15/CPS96_15.xlsx")

# data: 1996
CPS96 <- CPS96_15[CPS96_15$year == 1996,]

# data: 2015
CPS15 <- CPS96_15[CPS96_15$year == 2015,]

# i. sample mean # ii. standard error # iii. 95% CI
E31a <- function(x){
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

tapply(CPS96_15$ahe, CPS96_15$year, E31a)

# iv. 95% confidence interval for difference

t.test(CPS15$ahe, CPS96$ahe, 
       alternative = c("two.sided"),
       mu = 0, # H0
       var.equal = FALSE,
       conf.level = 0.95) # alpha = 0.05

#####
# b.
#####

# data: 1996
CPS96 <- CPS96_15[CPS96_15$year == 1996,]

# CPI
CPI_96 <- 156.9
CPI_15 <- 237

# adjusted 1996 AHE in $2015 
ahe_adjust <- CPS96$ahe * (CPI_15/CPI_96)

# data: 1996 including adjusted 1996 AHE in $2015 
CPS96 <- cbind(CPS96, ahe_adjust)

# i. sample mean
# ii. sample standard deviation
# iii. 95% confidence interval
# for adjusted 1996 AHE in $2015 
E31a(CPS96$ahe_adjust)

# iv. 95% confidence interval for difference
t.test(CPS15$ahe, CPS96$ahe_adjust, 
       alternative = c("two.sided"),
       mu = 0, # H0
       var.equal = FALSE,
       conf.level = 0.95) # alpha = 0.05







