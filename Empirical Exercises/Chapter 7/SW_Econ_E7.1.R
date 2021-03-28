##### SW_Econ_E7.1

#####
# a.
#####

library(kableExtra); library(stargazer)
library(ggplot2); library(dplyr);
library(jtools); library(ggstance); 
library(broom.mixed); library(huxtable)

# import data set
library(readxl)
Birthweight_Smoking <- read_excel("TA Sessions/Empirical Exercises/Chapter 7/Birthweight_Smoking/Birthweight_Smoking.xlsx")

# variable names
colnames(Birthweight_Smoking)

# https://cran.r-project.org/web/packages/stargazer/stargazer.pdf
# https://www.jakeruss.com/cheatsheets/stargazer/
# https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf

library(sandwich); library(lmtest)

# Model 1
E71_M1 <- lm(formula = birthweight ~ smoker, 
             data = Birthweight_Smoking)

# Model 2
E71_M2 <- lm(formula = birthweight ~ smoker + alcohol + nprevist,
             data = Birthweight_Smoking)

# Model 3
E71_M3 <- lm(formula = birthweight ~ smoker + alcohol + nprevist + unmarried, 
             data = Birthweight_Smoking)

# Adjust standard errors
cov1 <- vcovHC(E71_M1, type = "HC1")
rb_se1 <- sqrt(diag(cov1))

cov2 <- vcovHC(E71_M2, type = "HC1")
rb_se2 <- sqrt(diag(cov2))

cov3 <- vcovHC(E71_M3, type = "HC1")
rb_se3 <- sqrt(diag(cov3))

# output table
stargazer(E71_M1, E71_M2, E71_M3, 
          type = "text",
          se = list(rb_se1, rb_se2, rb_se3),
          digits = 4)

#####
# b.
#####

# https://stats.stackexchange.com/questions/117052/replicating-statas-robust-option-in-r
library(estimatr)

# Model 1 # use HC1
E71_M1n <- lm_robust(formula = birthweight ~ smoker, 
                     data = Birthweight_Smoking,
                     se_type = "stata")

# Model 2
E71_M2n <- lm_robust(formula = birthweight ~ smoker + alcohol + nprevist,
                     data = Birthweight_Smoking,
                     se_type = "stata")

# Model 3
E71_M3n <- lm_robust(formula = birthweight ~ smoker + alcohol + nprevist + unmarried, 
                     data = Birthweight_Smoking,
                     se_type = "stata")

export_summs(E71_M1n, E71_M2n, E71_M3n,
             digits = 4,
             ci_level = 0.95,
             error_format = "[{conf.low}, {conf.high}]",
             model.names = c("Model (1)", "Model (2)", "Model (3)"))

#####
# c.
#####

(E71_M2$coefficients[2] - E71_M1$coefficients[2]) / E71_M1$coefficients[2]

#####
# d.
#####

(E71_M3$coefficients[2] - E71_M2$coefficients[2]) / E71_M2$coefficients[2]

#####
# f.
#####

# Model 4
E71_M4 <- lm(formula = birthweight ~ smoker + alcohol + nprevist + unmarried + age + educ, 
             data = Birthweight_Smoking)

# Adjust standard errors
cov4 <- vcovHC(E71_M4, type = "HC1")
rb_se4 <- sqrt(diag(cov4))

# output table
stargazer(E71_M3, E71_M4,
          type = "text",
          column.labels=c("Model (3)","Model (4)"),
          se = list(rb_se3, rb_se4),
          digits = 4)












