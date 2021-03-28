##### SW_Econ_E7.2

#####
# b.
#####

# https://cran.r-project.org/web/packages/stargazer/stargazer.pdf
# https://www.jakeruss.com/cheatsheets/stargazer/
# https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf

library(sandwich); library(lmtest) 
library(stargazer)
library(car)

# import data 
library(readxl)
Earnings_and_Height <- read_xlsx("TA Sessions/Empirical Exercises/Chapter 7/Earnings_and_Height/Earnings_and_Height.xlsx")

# create dummay variables
LT_HS <- c()

for (i in 1:length(Earnings_and_Height$educ)){
  if (Earnings_and_Height$educ[i] < 12){
    LT_HS[i] <- c(1) 
  } else {
    LT_HS[i] <- c(0) 
  }
}

HS <- c()

for (i in 1:length(Earnings_and_Height$educ)){
  if (Earnings_and_Height$educ[i] == 12){
    HS[i] <- c(1) 
  } else {
    HS[i] <- c(0) 
  }
}

Some_Col <- c()

for (i in 1:length(Earnings_and_Height$educ)){
  if (Earnings_and_Height$educ[i] > 12 & Earnings_and_Height$educ[i] < 16){
    Some_Col[i] <- c(1) 
  } else {
    Some_Col[i] <- c(0) 
  }
}


College <- c()

for (i in 1:length(Earnings_and_Height$educ)){
  if (Earnings_and_Height$educ[i] >= 16){
    College[i] <- c(1) 
  } else {
    College[i] <- c(0) 
  }
}

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

#####
# b. # i.
#####

(E72_M2$coefficients[2] - E72_M1$coefficients[2]) / E72_M1$coefficients[2]

#####
# b. # iii.
#####

linearHypothesis(E72_M2, 
                 c("LT_HS = 0", "HS = 0", "Some_Col = 0"),
                 test = "F")

#####
# c.
#####

# data # men
Earnings_and_Height1 <- Earnings_and_Height[Earnings_and_Height$sex == 1,]

# Model 3
E72_M3 <- lm(formula = earnings ~ height, 
             data = Earnings_and_Height1)

# Mdoel 4
E72_M4 <- lm(formula = earnings ~ height + LT_HS + HS + Some_Col, 
             data = Earnings_and_Height1)

# Adjust standard errors
cov3 <- vcovHC(E72_M3, type = "HC1")
rb_se3 <- sqrt(diag(cov3))

cov4 <- vcovHC(E72_M4, type = "HC1")
rb_se4 <- sqrt(diag(cov4))

# output table
stargazer(E72_M3, E72_M4,
          type = "text",
          column.labels = c("Men", "Men"),
          se = list(rb_se3, rb_se4),
          digits = 4)

#####
# c. # i.
#####

(E72_M4$coefficients[2] - E72_M3$coefficients[2]) / E72_M3$coefficients[2]

#####
# c. # ii.
#####

linearHypothesis(E72_M4, 
                 c("LT_HS = 0", "HS = 0", "Some_Col = 0"),
                 test = "F")

