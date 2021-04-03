##### SW_Econ_E8.1

#####
# a.
#####

### import data set
library(readxl)
path <- c("/Users/chi-yuan/Documents/NTU ECON - Spring 2021/ECON 2023 - Econometrics (Undergraduate)/TA Sessions/Empirical Exercises/Chapter 8/lead_mortality")
Lead_Mortality <- read_excel(paste(path, "/lead_mortality.xlsx", sep = ""))

### average Inf for nonlead (pipe = 0) and lead pipes (pipe = 1)
tapply(Lead_Mortality$infrate, Lead_Mortality$lead, mean)

### data: lead pipes (pipe = 1)
Lead_Mortality_lead1 <- Lead_Mortality[Lead_Mortality$lead == 1,]

### data: nonlead (pipe = 0)
Lead_Mortality_lead0 <- Lead_Mortality[Lead_Mortality$lead == 0,]

### t test for difference
t.test(Lead_Mortality_lead1$infrate, Lead_Mortality_lead0$infrate,
       alternative = c("two.sided"),
       mu = 0, # H0
       var.equal = FALSE,
       conf.level = 0.95)

#####
# b. # i.
#####

### regression
E81bi <- lm(formula = infrate ~ lead + ph + lead*ph,
            data = Lead_Mortality)

### output table
library(stargazer)

stargazer(E81bi,
          type = "text",
          digits = 4)

#####
# b. # ii.
#####

### lead = 0
E81bii0 <- lm(formula = infrate ~ ph,
              data = Lead_Mortality_lead0)

### lead = 1
E81bii1 <- lm(formula = infrate ~ ph,
              data = Lead_Mortality_lead1)

plot(x = Lead_Mortality$ph, 
     y = Lead_Mortality$infrate,
     pch = 16, # filled circle
     col = "black",
     xlim = c(5.5, 9),
     ylim = c(0.1, 0.8),
     xlab = "pH",
     ylab = "Inf",
     main = "E8.1 (b) ii.")

abline(E81bii0, lwd = 2, col = "steelblue") 

abline(E81bii1, lwd = 2, col = "red") 

# https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/legend
legend("topright", 
       legend = c("Cities with lead pipes", "Cities without lead pipes"),     
       col = c("steelblue","red"), 
       lty = 1, 
       lwd = 2) 

#####
# b. # iii.
#####

library(car)

linearHypothesis(E81bi,
                 c("lead = 0", "lead:ph = 0"),
                 test = "F")

#####
# b. # v.
#####

### average value of pH
pH_mu <- mean(Lead_Mortality$ph)
pH_mu

### estimated effect of Lead
E81bv <- function(pH){
  diff <- E81bi$coefficients[2] + E81bi$coefficients[4]*pH
  
  return(diff)
}

### estimated effect of Lead # average value of pH
E81bv(pH_mu)

### standard deviation of pH
pH_sd <- sd(Lead_Mortality$ph)
pH_sd

### estimated effect of Lead # pH_mu - pH_sd
E81bv(pH_mu - pH_sd)

### estimated effect of Lead # pH_mu + pH_sd
E81bv(pH_mu + pH_sd)

#####
# b. # vi.
#####

Lead_Mortality$phnew <- Lead_Mortality$ph - 6.5

E81bvi <- lm(formula = infrate ~ lead + ph + lead*phnew,
             data = Lead_Mortality)

confint(E81bvi)[2,]




