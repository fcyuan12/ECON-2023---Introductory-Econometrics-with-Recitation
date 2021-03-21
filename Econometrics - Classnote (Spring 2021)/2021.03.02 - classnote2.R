# install new package which we want to use
#install.packages("readxl")

# call the package which we want to use
library(readxl)

# import data
# 1. Check whether the folder of file(s) which we use is in the same foler for r script file.
# 2. Copy the place of the file (get information).
# 3. Paste the place in "read_xlsx"

CPS96_15 <- read_xlsx("/Users/chi-yuan/Documents/NTU ECON - Spring 2021/ECON 2023 - Econometrics (Undergraduate)/Classnote/Econometrics - Classnote (Spring 2021)/CPS96_15/CPS96_15.xlsx")

# data: 1996
CPS96 <- CPS96_15[CPS96_15$year == 1996,]

# data: 2015
CPS15 <- CPS96_15[CPS96_15$year == 2015,]

# (a) i-iii
E31a <- function(x){
  # i. sample mean
  mu <- mean(x)
  
  # ii. standard error 
  se <- sd(x)/sqrt(length(x))
  
  # iii. 95% CI
  lower <- mu - qnorm(0.975, mean = 0, sd = 1)*se
  upper <- mu + qnorm(0.975, mean = 0, sd = 1)*se
  CI <- paste(lower, "-", upper)
  
  # create output table
  Table <- data.frame(mu, se, CI)
  colnames(Table) <- c("Mean", "Standard Error", "95% Confidence Interval")
  
  Table
  
}

tapply(CPS96_15$ahe, CPS96_15$year, E31a)

# (a) iv. 95% confidence interval for difference

t.test(CPS15$ahe, CPS96$ahe, 
       alternative = c("two.sided"),
       mu = 0, # H0
       var.equal = FALSE,
       conf.level = 0.95) # alpha = 0.05

# (b)

# CPI
CPI_96 <- 156.9
CPI_15 <- 237

# adjusted 1996 AHE in $2015 
ahe_adjust <- CPS96$ahe * (CPI_15/CPI_96)

# data: 1996 including adjusted 1996 AHE in $2015 
CPS96 <- cbind(CPS96, ahe_adjust)

E31a(CPS96$ahe_adjust)

# (b) iv. 95% confidence interval for difference
t.test(CPS15$ahe, CPS96$ahe_adjust, 
       alternative = c("two.sided"),
       mu = 0, # H0
       var.equal = FALSE,
       conf.level = 0.95) # alpha = 0.05

# data: high school in 2015
CPS15hs <- CPS15[CPS15$bachelor == 0, ]

# data: college in 2015
CPS15col <- CPS15[CPS15$bachelor == 1, ]

# (d) # i. 95% CI for AHE in high school # ii. 95% CI for AHE in college
tapply(CPS15$ahe, CPS15$bachelor, E31a)

# (d) # iii. 95% CI for difference
t.test(CPS15hs$ahe, CPS15col$ahe, 
       alternative = c("two.sided"),
       mu = 0, # H0
       var.equal = FALSE,
       conf.level = 0.95) # alpha = 0.05

# data: high school in 2015
CPS96hs <- CPS96[CPS96$bachelor == 0, ]

# data: college in 2015
CPS96col <- CPS96[CPS96$bachelor == 1, ]

# (e) i. 95% CI for AHE in high school # ii. 95% CI for AHE in college
tapply(CPS96$ahe_adjust, CPS96$bachelor, E31a)

# (e) iii. 95% CI for difference
t.test(CPS96col$ahe_adjust, CPS96hs$ahe_adjust, 
       alternative = c("two.sided"),
       mu = 0, # H0
       var.equal = FALSE,
       conf.level = 0.95) # alpha = 0.05

# (f) i.
E31fi <- function(x1, x2){
  # mean 
  mu1 <- mean(x1)
  mu2 <- mean(x2)
  
  # s: sample standard deviation
  SD1 <- sd(x1)
  SD2 <- sd(x2)
  
  # n: number of variables (observations)
  n1 <- length(x1)
  n2 <- length(x2)
  
  # differcen in mean
  mu <- mu2 - mu1
  
  # difference in standard error
  se <- sqrt(SD1^2/n1 + SD2^2/n2)
  
  # 95% CI
  lower <- mu - qnorm(0.975, mean = 0, sd = 1)*se
  upper <- mu + qnorm(0.975, mean = 0, sd = 1)*se
  CI <- paste(lower, "-", upper)
  
  # create output table
  Table <- data.frame(mu, se, CI)
  colnames(Table) <- c("Mean", "Standard Error", "95% Confidence Interval")
  
  Table
}

E31fi(CPS96hs$ahe_adjust, CPS15hs$ahe)

t.test(CPS15hs$ahe, CPS96hs$ahe_adjust, 
       alternative = c("two.sided"),
       mu = 0, # H0
       var.equal = FALSE,
       conf.level = 0.95) # alpha = 0.05

# hypotheses:
# 1.CI:
# (i) if 0 is in CI, we do not reject H0
# (ii) if 0 is not in CI, we reject H0

# 2.p-value
# (i) if p-value > significance level, we do not reject H0
# (ii) if p-value <= significance level, we reject H0

# 3.test statistics
# (i) if |test statistics| >= critical value, we reject H0
# (ii) if |test statistics| < critical value, we do not reject H0

# (f) ii.
E31fi(CPS96col$ahe_adjust, CPS15col$ahe)

t.test(CPS15col$ahe, CPS96col$ahe_adjust, 
       alternative = c("two.sided"),
       mu = 0, # H0
       var.equal = FALSE,
       conf.level = 0.95) # alpha = 0.05

# (f) iii.
E31fiii <- function(x11, x12, x21, x22){
  # mean
  mu11 <- mean(x11)
  mu12 <- mean(x12)
  mu21 <- mean(x21)
  mu22 <- mean(x22)
  
  # mu
  mu1 <- mu11 - mu12
  mu2 <- mu21 - mu22
  
  # sample standard deviation
  SD11 <- sd(x11)
  SD12 <- sd(x12)
  SD21 <- sd(x21)
  SD22 <- sd(x22)
  
  # n: numbers of observations
  n11 <- length(x11)
  n12 <- length(x12)
  n21 <- length(x21)
  n22 <- length(x22)
  
  SD1 <- sqrt(SD11^2/n11 + SD12^2/n12)
  SD2 <- sqrt(SD21^2/n21 + SD22^2/n22)
  
  # difference in mean
  mu <- mu2 - mu1
  
  # difference in standard error
  se <- sqrt(SD1^2 + SD2^2)
  
  # 95% CI
  lower <- mu - qnorm(0.975, mean = 1, sd = 1)*se
  upper <- mu + qnorm(0.975, mean = 1, sd = 1)*se
  CI <- paste(lower, "-", upper)
  
  # create output table
  Table <- data.frame(mu, se, CI)
  colnames(Table) <- c("Mean", "Standard Error", "95% Confidence Interval")
  
  Table
  
}

E31fiii(CPS96col$ahe_adjust, CPS96hs$ahe_adjust, CPS15col$ahe, CPS15hs$ahe)





