##### SW_Econ_E6.2

#####
# a.
#####

library(ggplot2); library(dplyr);
library(jtools); library(ggstance); 
library(broom.mixed); library(huxtable)

# import data 
library(readxl)
Growth <- read_excel("TA Sessions/Empirical Exercises/Chapter 6/Growth/Growth.xlsx")

E62a <- function(x){
  # meam
  mu <- mean(x)
  
  # standard deviation
  SD <- sd(x)
  
  # minimum
  MIN <- min(x)
  
  # maximum
  MAX <- max(x)
  
  Table <- data.frame(mu, SD, MIN, MAX)
  
  colnames(Table) <- c("Mean", "Standard Deviation", "Minimum", "Maximum")
  
  Table
  
}

E62a_output <- as.list(matrix(ncol = 4))



for (i in 2:ncol(Growth)){
  E62a_output[[i]] <- E62a(Growth[[i]])
  
}

names(E62a_output) <- variable.names(Growth)

E62a_output


#####
# b.
#####

E62b <- lm(growth ~ tradeshare + yearsschool + rev_coups + assasinations + rgdp60, data = Growth)

summ(E62b)

#####
# c.
#####

# Sample means cross the sample linear regression.
mean(Growth$growth)

# predict value
E62c <- function(x){
  E62b$coefficients %*% matrix(c(1, x), ncol = 1)
  
}

E62c_X <- c(mean(Growth$tradeshare),
            mean(Growth$yearsschool),
            mean(Growth$rev_coups),
            mean(Growth$assasinations),
            mean(Growth$rgdp60))

E62c(E62c_X)

#####
# d.
#####

E62d_X <- c(mean(Growth$tradeshare) + sd(Growth$tradeshare),
            mean(Growth$yearsschool),
            mean(Growth$rev_coups),
            mean(Growth$assasinations),
            mean(Growth$rgdp60))

E62c(E62d_X)

#####
# e.
#####

sum(Growth$oil == 1)


