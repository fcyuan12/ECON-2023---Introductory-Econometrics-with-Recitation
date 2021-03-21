# install read 
# xlsx package
#install.packages("xlsx")

# call the package 
# which we use now
library(readxl)

# /Users/chi-yuan/Documents/NTU ECON - Spring 2021/ECON 2023 - Econometrics (Undergraduate)/Classnote/Econmetrics - Classnote (Spring 2021)
CPS96_15 <- read_xlsx("CPS96_15/CPS96_15.xlsx")

CPS96_15 <- read_xlsx("/Users/chi-yuan/Documents/NTU ECON - Spring 2021/ECON 2023 - Econometrics (Undergraduate)/Classnote/Econmetrics - Classnote (Spring 2021)/CPS96_15/CPS96_15.xlsx")

# if you don't import "xlsx" file sucessfully,
# download "csv" file on NTU Cool
# 1. copy the file place (get information)
# 2. paste the place on "read.csv" command
# 3. add the file name after the place
# 4. run it

CPS96_15_csv <- read.csv("/Users/chi-yuan/Documents/NTU ECON - Spring 2021/ECON 2023 - Econometrics (Undergraduate)/Classnote/Econmetrics - Classnote (Spring 2021)/CPS96_15/CPS96_15.csv")

# question f
# f. (i)
# data: 1996
CPS96 <- CPS96_15[CPS96_15$year == 1996,]

# data: 2015
CPS15 <- CPS96_15[CPS96_15$year == 2015,]

# CPI
CPI_96 <- 156.9
CPI_15 <- 237

# adjusted 1996 AHE in $2015 
ahe_adjust <- CPS96$ahe * (CPI_15/CPI_96)

# data: 1996 including adjusted 1996 AHE in $2015 
CPS96 <- cbind(CPS96, ahe_adjust)

# data: 1996 high school
CPS96hs <- CPS96[CPS96$bachelor == 0, ]
# data: 2015 high school
CPS15hs <- CPS15[CPS15$bachelor == 0, ]
# data: 1996 college
CPS96col <- CPS96[CPS96$bachelor == 1, ]
# data: 2015 college
CPS15col <- CPS15[CPS15$bachelor == 1, ]

# write an function 
# which creates mean, standard error,
# and 95% confidence interval
E31fi <- function(x1, x2){
  # mean
  mu1 <- mean(x1); mu2 <- mean(x2)
  
  # s
  SD1 <- sd(x1); SD2 <- sd(x2)
  
  # n
  n1 <- length(x1); n2 <- length(x2)
  
  # difference in mean
  mu <- mu2 - mu1
  
  # difference in standard error
  se <- sqrt(SD1^2/n1 + SD2^2/n2)
  
  # 95% confidence interval
  lower <- round(mu - qnorm(0.975, mean = 0, sd = 1)*se, digit = 4)
  upper <- round(mu + qnorm(0.975, mean = 0, sd = 1)*se, digit = 4)
  CI <- paste(lower, "-"  ,upper)
  
  Table <- data.frame(mu, se, CI)
  colnames(Table) <- c("Mean", "Standard Error", "95% Confidence Interval")
  
  Table  
  
}

#E31fi(CPS96hs$ahe_adjust, CPS15hs$ahe)
E31fi(CPS96hs$ahe, CPS15hs$ahe)

# if 0 is in CI, we do "not" reject H0
# if 0 is not in CI, we reject H0

# because 0 is not in CI, we reject H0

# f.(ii)
E31fi(CPS96col$ahe, CPS15col$ahe)
