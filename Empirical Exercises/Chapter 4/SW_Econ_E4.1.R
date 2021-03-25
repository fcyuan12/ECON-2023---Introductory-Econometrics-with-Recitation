##### SW_Econ_E4.1

#####
# a.
#####

# import data 
library(readxl)
Growth <- read_xlsx("TA Sessions/Empirical Exercises/Chapter 4/Growth/Growth.xlsx")

plot(x = Growth$tradeshare, 
     y = Growth$growth,
     pch = 16, # filled circle
     col = "black",
     xlim = c(0, 2),
     ylim = c(-3, 8),
     xlab = "TradeShare",
     ylab = "Growth",
     main = "E4.1 (a)")

#####
# b.
#####

Growth[Growth$country_name =="Malta",]

#####
# c.
#####

# regression 
E41c <- lm(formula = growth ~ tradeshare, data = Growth)

# estimated intercept, estimated slope
summary(E41c)

# predict value
E41c_predict <- function(x){
  E41c$coefficients %*% matrix(c(1, x), ncol = 1)
  
}

# predict value: tradeshare = 0.5
E41c_predict(0.5)

# predict value: tradeshare = 1.0
E41c_predict(1)

#####
# d.
#####

# excluding the data from Malta
Growth_n <- Growth[Growth$country_name !="Malta",]

# regression 
E41d <- lm(formula = growth ~ tradeshare, data = Growth_n)

# estimated intercept, estimated slope
summary(E41d)

# predict value
E41d_predict <- function(x){
  E41d$coefficients %*% matrix(c(1, x), ncol = 1)
  
}

# predict value: tradeshare = 0.5
E41d_predict(0.5)

# predict value: tradeshare = 1.0
E41d_predict(1)

#####
# e.
#####

plot(x = Growth$tradeshare, 
     y = Growth$growth,
     pch = 16, # filled circle
     col = "black",
     xlim = c(0, 2),
     ylim = c(-3, 8),
     xlab = "TradeShare",
     ylab = "Growth",
     main = "E4.1 (e)")

# with Malta
abline(E41c, lwd = 2, col = "steelblue") 
text(1.5, 6, "OLS regression line including Malta", col = "steelblue")

# without Malta
abline(E41d, lwd = 2, col = "red") 
text(1.5, 0, "OLS regression line excluding Malta", col = "red")






