# install some useful
# packages
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("jtools")
#install.packages("ggstance")
#install.packages("broom.mixed")
#install.packages("huxtable")

library(ggplot2); library(dplyr);
library(jtools); library(ggstance);
library(broom.mixed); library(huxtable)

# read_excel:
# import xls or xlsx file
library(readxl)
Birthweight_Smoking <- read_excel("Birthweight_Smoking/Birthweight_Smoking.xlsx")

# a.
E61a <- lm(formula = birthweight ~ smoker,
           data = Birthweight_Smoking)

summary(E61a)

# OLS: ordinary least squares
summ(E61a, 
     confint = TRUE)

# b. 
E61b <- lm(birthweight ~ smoker + alcohol + nprevist, 
           data = Birthweight_Smoking)

summ(E61b, confint = TRUE, digits = 4)

export_summs(E61a, E61b,
             model.names = c("Model (a)", "Model (b)"))

# b # iii.
# Yhat = 3051.25 - 217.58 smoker
#                - 30.49 alcohol
#                + 34.07 nprevist
Yhat <- 3051.25 - 217.58*1 - 30.49*0 + 34.07*8
Yhat

# different input --- different output
# write function(s)

# find structure 
str(E61b)

E61b$coefficients
# Model:
# Yhat = b0hat*X0 + b1hat*X1 + b2hat*X2 + b3hat*X3
# Define X0 = 1
# eg.input = (1,1,0,8)
value <- c()
bhat <- E61b$coefficients

# these function has bug # I should debug
E61biii <- function(x){
  input <- c(1, x)
  
  for (i in 1:length(input)){
    # value[1] = bhat[1]*input[1]...beta0hat*1
    # value[2] = bhat[2]*input[2]...beta1hat*x1
    # value[3] = bhat[3]*input[3]...beta2hat*x2
    # value[4] = bhat[4]*input[4]...beta3hat*x3
    value[i] <- bhat[i]*input[i] 
  }
  
}

X <- c(1,0,8)
E61biii(X)

###

E61biii <- function(x){
  E61b$coefficients %*% matrix(c(1, x), ncol = 1)
  
}

# predict value: 
# smoker = 1, alcohol = 0, nprevist = 8
E61biii(c(1, 0, 8))


# b. # v. control variable
# Some details are in Section 6.8.

# c.
# Step 1: regress X1 on X2, X3, ..., Xk, and residuals = X1 tilde 
E61c1 <- lm(smoker ~ alcohol + nprevist, data = Birthweight_Smoking)
smoker <- E61c1$residuals

# Step 2: regress Y on X2, X3, ..., Xk, and residuals = Y tilde 
E61c2 <- lm(birthweight ~ alcohol + nprevist, data = Birthweight_Smoking)
birthweight <- E61c2$residuals

# Step 3: regress Y tilde on X1 tilde
E61c3 <- lm(birthweight ~ smoker)

export_summs(E61b, E61c3,
             model.names = c("Model (b)", "Model (c)"))

# d.
E61d <- lm(birthweight ~ smoker + alcohol + tripre0 + tripre2 + tripre3, 
           data = Birthweight_Smoking)

summ(E61d, confint = TRUE, digits = 4)

# d. # i. dummy variables trap
# if you have k categories,
# you can only use k-1 dummy variables

# ii. difference in means is 
# estimated coefficient
# for dummy variable with
# 2 categories



