# if you cannot install
# "readxl",
# you can change to 
# install "tidyverse"
#install.packages("tidyverse")

library(readxl)

Growth <- read_xlsx("/Users/chi-yuan/Documents/NTU ECON - Spring 2021/ECON 2023 - Econometrics (Undergraduate)/Classnote/Econometrics - Classnote (Spring 2021)/Growth/Growth.xlsx")

# a. 
plot(x = Growth$tradeshare,
     y = Growth$growth,
     # xlab: x label
     xlab = "Trade Share",
     # ylab: y label
     ylab = "Growth",
     # main: figure name
     main = "E4.1 (a)"
     )

# b 
Malta <- Growth[Growth$country_name == "Malta",]
Malta

# c
# lm: linear model
E41c <- lm(formula = growth ~ tradeshare, 
           data = Growth)
E41c

summary(E41c)

# Notes for E41c:
# estimated coefficient (SE):
# beta1 hat = 2.3064 (0.7735)
# beta0 hat = 0.6403 (0.7735)

# H0: beta1 = 0 v.s. 
# H1: beta1 neq 0
# Let the significance level 5%.
# Because p-value = 0.00407 < 5%,
# we reject H0.

# R-squared:  0.1237

# Yhat = beta0 hat + beta1 hat * X1
# Yhat = 0.6403 + 2.3064 X1

Yhat1 <- 0.6403 + 2.3064*0.5
Yhat1

Yhat2 <- 0.6403 + 2.3064*1
Yhat2

# d. 
# step 1: delete outlier
# step 2: run linear regression again
Growth_n <- Growth[Growth$country_name != "Malta", ]

E41d <- lm(formula = growth ~ tradeshare, 
           data = Growth_n)

summary(E41d)

# Notes on E41d:
# estimated coefficient:
# beta1 hat = 1.6809 
# beta0 hat = 0.9574

Yhat1new <- 0.9574 + 1.6809*0.5
Yhat1new

Yhat2new <- 0.9574 + 1.6809*1
Yhat2new

# e
plot(x = Growth$tradeshare,
     y = Growth$growth,
     # xlab: x label
     xlab = "Trade Share",
     # ylab: y label
     ylab = "Growth",
     # main: figure name
     main = "E4.1 (e)")
abline(E41c, lwd = 2, col = "steelblue") 
# OLS: ordinary least squares
text(1.5, 6, "OLS regression line including Malta", col = "steelblue")

abline(E41d, lwd = 2, col = "red") 
text(1.5, 0, "OLS regression line excluding Malta", col = "red")
