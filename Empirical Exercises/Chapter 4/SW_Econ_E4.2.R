##### SW_Econ_E4.2

#####
# a.
#####

# import data 
library(readxl)
Earnings_and_Height <- read_xlsx("TA Sessions/Empirical Exercises/Chapter 4/Earnings_and_Height/Earnings_and_Height.xlsx")

median(Earnings_and_Height$height)

#####
# b.
#####

# create "group" variable
group <- c()

for (i in 1:length(Earnings_and_Height$height)){
  if (Earnings_and_Height$height[i] <= 67){
    # group 0: height <= 67
    group[i] <- c(0) 
  } else {
    # group 1: height > 67
    group[i] <- c(1) 
  }
}

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

# i. # ii.
tapply(Earnings_and_Height$earnings, Earnings_and_Height$group, E42b)

# height <= 67
Earnings_and_Height_i <- Earnings_and_Height[Earnings_and_Height$height <= 67, ]

# height > 67
Earnings_and_Height_ii <- Earnings_and_Height[Earnings_and_Height$height > 67, ]

# iii. 95% CI for difference
t.test(Earnings_and_Height_ii$earnings, Earnings_and_Height_i$earnings, 
       alternative = c("two.sided"),
       mu = 0, # H0
       var.equal = FALSE,
       conf.level = 0.95) # alpha = 0.05

#####
# c.
#####

plot(x = Earnings_and_Height$height, 
     y = Earnings_and_Height$earnings,
     pch = 16, # filled circle
     col = "black",
     xlim = c(45, 90),
     ylim = c(0, 85000),
     xlab = "height",
     ylab = "earnings",
     main = "E4.2 (c)")

#####
# d.
#####

# regression 
E42d <- lm(formula = earnings ~ height, data = Earnings_and_Height)

# i. estimated intercept, estimated slope
summary(E42d)

# predict value
E42d_predict <- function(x){
  E42d$coefficients %*% matrix(c(1, x), ncol = 1)
  
}

# ii. predict value: height = 67
E42d_predict(67)

# ii. predict value: height = 70
E42d_predict(70)

# ii. predict value: height = 65
E42d_predict(65)

#####
# e.
#####

# translates from inches to cm
height_cm <- cm(Earnings_and_Height$height)

Earnings_and_Height <- cbind(Earnings_and_Height, height_cm)

# regression 
E42e <- lm(formula = earnings ~ height_cm, data = Earnings_and_Height)

# i. estimated slope # ii. estimated intercept
# iii. R^2 # iv. SE
summary(E42e)

#####
# f.
#####

# female
Earnings_and_Height_f <- Earnings_and_Height[Earnings_and_Height$sex == 0, ]

# regression 
E42f <- lm(formula = earnings ~ height, data = Earnings_and_Height_f)

# i. estimated slope # ii.
summary(E42f)

#####
# g.
#####

# male
Earnings_and_Height_g <- Earnings_and_Height[Earnings_and_Height$sex == 1, ]

# regression 
E42g <- lm(formula = earnings ~ height, data = Earnings_and_Height_g)

# i. estimated slope # ii.
summary(E42g)






