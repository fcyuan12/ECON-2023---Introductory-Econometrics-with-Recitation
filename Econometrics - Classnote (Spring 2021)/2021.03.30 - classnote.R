# import data set
library(readxl)

path <- c("/Users/chi-yuan/Documents/NTU ECON - Spring 2021/ECON 2023 - Econometrics (Undergraduate)/Classnote/Econometrics - Classnote (Spring 2021)/birthweight_smoking")
Birthweight_Smoking <- read_excel(paste(path, "/Birthweight_Smoking.xlsx", sep = ""))

# variable names
colnames(Birthweight_Smoking)

# Model 1
E71_M1 <- lm(formula = birthweight ~ smoker,
             data = Birthweight_Smoking)

# Model 2
E71_M2 <- lm(formula = birthweight ~ smoker + alcohol + nprevist,
             data = Birthweight_Smoking)

# Model 3
E71_M3 <- lm(formula = birthweight ~ smoker + alcohol + nprevist + unmarried, 
             data = Birthweight_Smoking)


#install.packages("stargazer")
library(kableExtra); library(stargazer)
library(ggplot2); library(dplyr);
library(jtools); library(ggstance); 
library(broom.mixed); library(huxtable)
library(sandwich); library(lmtest)

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

# b
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
             #error_format = "[{conf.low}, {conf.high}]",
             model.names = c("Model (1)", "Model (2)", "Model (3)"))

(E71_M2$coefficients[2] - E71_M1$coefficients[2]) / E71_M1$coefficients[2]

# f
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

# E7.2
# b
# import data 
library(readxl)
path <- c("/Users/chi-yuan/Documents/NTU ECON - Spring 2021/ECON 2023 - Econometrics (Undergraduate)/Classnote/Econometrics - Classnote (Spring 2021)/")

Earnings_and_Height <- read_xlsx(paste(path,"Earnings_and_Height/Earnings_and_Height.xlsx", sep = ""))

table(Earnings_and_Height$educ)

# LT_HS=1 if educ<12, 0 otherwise
LT_HS <- c()

for (i in 1:length(Earnings_and_Height$educ)){
  if (Earnings_and_Height$educ[i] < 12){
    LT_HS[i] <- c(1) 
  } else {
    LT_HS[i] <- c(0) 
  }
}

# HS=1 if educ=12 , 0 otherwise
HS <- c()

for (i in 1:length(Earnings_and_Height$educ)){
  if (Earnings_and_Height$educ[i] == 12){
    HS[i] <- c(1) 
  } else {
    HS[i] <- c(0) 
  }
}

# Some_Col=1 if 12<educ<16, 0 otherwise
Some_Col <- c()

for (i in 1:length(Earnings_and_Height$educ)){
  if (Earnings_and_Height$educ[i] > 12 & Earnings_and_Height$educ[i] < 16){
    Some_Col[i] <- c(1) 
  } else {
    Some_Col[i] <- c(0) 
  }
}

# College=1 if educ≥16, 0 otherwise
College <- c()

for (i in 1:length(Earnings_and_Height$educ)){
  if (Earnings_and_Height$educ[i] >= 16){
    College[i] <- c(1) 
  } else {
    College[i] <- c(0) 
  }
}

# merge original data and dummy variables
Earnings_and_Height <- cbind(Earnings_and_Height, LT_HS, HS, Some_Col, College)

# correlation matrix
Earnings_and_Height_interest <- Earnings_and_Height[,c("earnings", "height", "LT_HS", "HS", "Some_Col")]

cor(Earnings_and_Height_interest)

# data # women
Earnings_and_Height0 <- Earnings_and_Height[Earnings_and_Height$sex == 0,]

# Model 1
E72_M1 <- lm(formula = earnings ~ height, 
             data = Earnings_and_Height0)

# Mdoel 2
E72_M2 <- lm(formula = earnings ~ height + LT_HS + HS + Some_Col, 
             data = Earnings_and_Height0)

# Adjust standard errors
cov1 <- vcovHC(E72_M1, type = "HC1")
rb_se1 <- sqrt(diag(cov1))

cov2 <- vcovHC(E72_M2, type = "HC1")
rb_se2 <- sqrt(diag(cov2))

# output table
stargazer(E72_M1, E72_M2,
          type = "text",
          column.labels = c("Women", "Women"),
          se = list(rb_se1, rb_se2),
          digits = 4)

# How can we dummy variable(s) trap?
# 1. Run k-1 dummy variables.
# 2. Use no intercept model.

library(lmtest) 
library(car)
linearHypothesis(E72_M2, 
                 c("LT_HS = 0", "HS = 0", "Some_Col = 0"),
                 test = "F")



