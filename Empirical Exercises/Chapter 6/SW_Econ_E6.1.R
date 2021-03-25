##### SW_Econ_E6.1

#####
# a.
#####

library(ggplot2); library(dplyr);
library(jtools); library(ggstance); 
library(broom.mixed); library(huxtable)

# import data 
library(readxl)
Birthweight_Smoking <- read_excel("TA Sessions/Empirical Exercises/Chapter 6/Birthweight_Smoking/Birthweight_Smoking.xlsx")

E61a <- lm(birthweight ~ smoker, data = Birthweight_Smoking)

summ(E61a, confint = TRUE, digits = 4)

#####
# b.
#####

# nprevist: total number of prenatal visits
E61b <- lm(birthweight ~   smoker + alcohol + nprevist, data = Birthweight_Smoking)

summ(E61b, confint = TRUE, digits = 4)

#####
# b. # ii.
#####

export_summs(E61a, E61b,
             model.names = c("Model (a)", "Model (b)"))

#####
# b. # iii.
#####

# predict value
E61biii <- function(x){
  E61b$coefficients %*% matrix(c(1, x), ncol = 1)
  
}

# predict value: 
# smoker = 1, alcohol = 0, nprevist = 8
E61biii(c(1, 0, 8))

#####
# c.
#####

# Step 1: regress X1 on X2, X3, ..., Xk, and residuals = X1 tilde 
E61c1 <- lm(smoker ~ alcohol + nprevist, data = Birthweight_Smoking)
smoker <- E61c1$residuals

# Step 2: regress Y on X2, X3, ..., Xk, and residuals = Y tilde 
E61c2 <- lm(birthweight ~ alcohol + nprevist, data = Birthweight_Smoking)
birthweight <- E61c2$residuals

# Step 3: regress Y tilde on X1 tilde
E61c3 <- lm(birthweight ~ smoker)

summ(E61c3)

# comparison
export_summs(E61b, E61c3,
             model.names = c("Model (b)", "Model (c)"))



#####
# d.
#####

E61d <- lm(birthweight ~ smoker + alcohol + tripre0 + tripre2 + tripre3, data = Birthweight_Smoking)

summ(E61d, confint = TRUE, digits = 4)

#####
# d. # i.
#####

E61di <- lm(birthweight ~ smoker + alcohol + tripre0 + tripre1 + tripre2 + tripre3, data = Birthweight_Smoking)

summ(E61di, confint = TRUE, digits = 4)

#####
# d. # iv.
#####

export_summs(E61b, E61d, 
             model.names = c("Model (b)", "Model (d)"))
