# Return period based on gage data
library(dplyr)
library(readr)
library(lubridate)
library(e1071)
library(devtools)
install_github("LimpopoLab/hydrostats", force = TRUE)
library(hydrostats)

## ANNUAL MAXIMUM PRECIPITATION EXAMPLE
# This is an example from Chow, Maidment, and Mays, 1988, Applied Hydrology
x <- read_csv("ChowMaidmentMays_Ex12_2_1.csv")
T_R <- 50 # return period in years (in this case)
Fx <- 1 - (1/T_R)
y <- evi(x$max_precip_in,Fx)

## ANNUAL FLOOD EXAMPLE 1
# This is an example from Chow, Maidment, and Mays, 1988, Applied Hydrology
z <- read_csv("ChowMaidmentMays_Ex12_3_3.csv")
z <- rename(z, q = AnnualMaxDischarge_cfs) # remember, units are cfs

# Compare data and log-transformed data
hist(z$AnnualMaxDischarge_cfs)
z$log_q <- log(z$AnnualMaxDischarge_cfs, 10) # this is log base 10
hist(z$log_q) # this is much closer to a normal distribution

## LOGNORMAL ANALYSIS
m <- mean(z$log_q)
s <- sd(z$log_q)
c <- skewness(z$log_q)
c1 <- skew(z$log_q) # uses built-in skewness calculation.  there is a difference between methods.

# Note, the reason the distribution is so important is because 
# the probabilities and return period are taken from the 
# distribution given the statistics, m and s ONLY.

T_R <- 50 # return period in years (in this case)
Fx <- 1 - (1/T_R)

# the random variable (mean 0, standard deviation 1) that 
# produces a cummulative probability, or probability of 
# non-exceedence, of Fx.

x <- qnorm(Fx)

y <- (x * s) + m
z <- 10^y # this is the T_R flood level

## log-Pearson type III analysis
# For this distribution, we use the skewness to determine the 
# inverse CDF.
T_R <- 50 # return period in years (in this case)
Fx <- 1 - (1/T_R)
x <- pt3(c,Fx)
y <- (x * s) + m
z <- 10^y # this is the T_R flood level

## EXAMPLE 2

z <- read_csv("mon_annual_peaks.csv")

x <- z$q # random variable, peak flow
y <- log(x) # log of data
m <- mean(y)
s <- sd(y)
c <- skewness(y)
yn <- (y-m)/s # normalized log variables
cdf <- pnorm(yn,0,1)
plot(x,cdf)
