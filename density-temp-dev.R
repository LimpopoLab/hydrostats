# Development of water density as a function of temperature.
# This function does not account for changes in atmospheric pressure.

library(matlib)

# Given the data from USGS: https://www.usgs.gov/special-topic/water-science-school/science/water-density?qt-science_center_objects=0#qt-science_center_objects 
# Temperature (degrees C)     Density (kg/m^3)
# 0	                        999.87
# 4	                        1000
# 4.4	                        999.99
# 10	                        999.75
# 15.6	                  999.07
# 21	                        998.02
# 26.7	                  996.69
# 32.2	                  995.1
# 37.8	                  993.18
# 48.9	                  988.7
# 60	                        983.38
# 71.1	                  977.29
# 82.2	                  970.56
# 93.3	                  963.33
# 100	                        958.65

t <- c(0, 4, 4.4, 10, 15.6, 21, 26.7, 32.2, 37.8, 48.9, 60, 71.1, 82.2, 93.3, 100)
r <- c(999.87, 1000, 999.99, 999.75, 999.07, 998.02, 996.69, 995.1, 993.18, 988.7, 983.38, 977.29, 970.56, 963.33, 958.65)

# Cubic regression by matrices: 
x <- array(1, dim = c(length(t),4))
x[1:length(t),2] <- t
x[1:length(t),3] <- t^2
x[1:length(t),4] <- t^3
y <- r
xT <- t(x)
c <- inv(xT %*% x) %*% xT %*% y
# coefficients: 
# 1008.450391
# -11.484011
# -1.619761
# -0.433760

# Regression by linear model:
model <- lm(r ~ poly(t, degree = 3))
summary(model)
# Call:
#       lm(formula = r ~ poly(t, 3))
# 
# Residuals:
#       Min       1Q   Median       3Q      Max 
# -0.10968 -0.03997 -0.00243  0.03855  0.09510 
# 
# Coefficients:
#                       Estimate    Std. Error  t value     Pr(>|t|)    
#       (Intercept)     988.23867    0.01718    57510.91    < 2e-16 ***
#       poly(t, 3)1     -51.99462    0.06655    -781.27     < 2e-16 ***
#       poly(t, 3)2     -12.11570    0.06655    -182.05     < 2e-16 ***
#       poly(t, 3)3     1.41870      0.06655    21.32       2.69e-10 ***
#       ---
#       Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.06655 on 11 degrees of freedom
# Multiple R-squared:      1,	Adjusted R-squared:      1 
# F-statistic: 2.147e+05 on 3 and 11 DF,  p-value: < 2.2e-16

# given t, the density should be given by:
c <- as.numeric(model$coefficients)
d <- c[1] + c[2]*t + c[3]*t^2 + c[4]*t^3

# Ultimately, this did not yield usable coefficients for the model.  MS Excel cubic fit were used in the final function.
