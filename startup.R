# Getting started
library(devtools)
install_github("LimpopoLab/hydrostats", force = TRUE)
library(hydrostats)
library(dplyr)
library(tidyr)
library(ggplot2)
library(latex2exp)

# To update documentation:
devtools::document()
# https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html

x <- ((c(1:41))/10)-2.1
Erf <- erf(x)
Erfc <- 1-erf(x)
inverse <- erfinv(Erf)

inv <- data.frame(x,inverse)

ggplot(inv) +
      geom_line(aes(x = x, y = x)) +
      geom_point(aes(x = x, y = inverse)) +
      labs(x = "x", y = TeX("Erf^{-1}(Erf(x))")) +
      xlim(c(-2,2)) + ylim(c(-2,2)) +
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(aspect.ratio = 1) +
      theme(axis.text = element_text(face = "plain", size = 12))


plot(x,inverse)
lines(c(-2,2),c(-2,2))

err <- data.frame(x,Erf,Erfc)
err <- pivot_longer(err, cols = c(Erf, Erfc), 
             names_to = "Function", 
             values_to = "Value")

ggplot(err, aes(x = x, y = Value)) +
      geom_line(aes(linetype = Function)) +
      labs(x = "x", y = "y") +
      xlim(c(-2,2)) + ylim(c(-2,2)) +
      scale_linetype_manual(values=c("solid", "dashed")) +
      theme(legend.position="right") +
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(aspect.ratio = 1) +
      theme(axis.text = element_text(face = "plain", size = 12))

# Inverse formula from Wikipedia:
# ((pi^0.5)/2) * (z + pi*(z^3)/12 + 7*(pi^2)*(z^5)/480 + 127*(pi^3)*(z^7)/40320 + 4369*(pi^4)*(z^9)/5806080 + 34807*(pi^5)*(z^11)/182476800)
# but that doesn't work either.

# From Wolfram (which still doesn't work):
# x <- erf(-2)
# su <- 0
# top <- 0
# bottom <- 0
# for (i in 1:21) {
#       k <- i - 1
#       top <- x^(2*k+1)
#       bottom <- factorial(k) * (2*k+1)
#       su <- su + (top / bottom)
# }
# t <- (2 / (pi^(0.5))) * su
#
# more tries: constants worked out ok, but rest of method failed.
# x <- z * (pi^(0.5)) / 2
# c <- array(NA, dim = 10)
# c[1] <- 1
# for (i in 2:10) {
#       current <- 0
#       k <- i - 1
#       for (j in 1:k) {
#             m <- j-1
#             top <- c[m+1] * c[k-m]
#             bottom <- (m+1)*(2*m+1)
#             current <- current + (top/bottom)
#       }
#       c[i] <- current
# }
# su <- 0
# for (i in 1:100) {
#       k <- i - 1
#       right <- x^(2*k+1)
#       den <- 2 * k + 1
#       su <- su + (right * c[i] / den)
# }
# erfinv <- su * 2 / (pi^0.5)
# return(erfinv)
# 
# From Strecok, A. (1968). On the calculation of the inverse of the error function. Mathematics of Computation<, 22(101), 144–158. https://doi.org/10.1090/S0025-5718-1968-0223070-2
# equation 8, table 2
# input: x
# c <- c(0.88622692545275801365,
#        0.23201366653465449355,
#        0.12755617530559795825,
#        0.08655212924154853373,
#        0.06495961774538541338,
#        0.05173128198461637411,
#        0.04283672065179734984,
#        0.03646592930853162632,
#        0.03168900502160544680,
#        0.02798063296499522473, #10
#        0.02502227584119834946,
#        0.02262986331889757443,
#        0.02060678037905900172,
#        0.01891821725077885446,
#        0.01747637056285654619,
#        0.01623150098768525128,
#        0.01514631506324780552,
#        0.01419231600250996415,
#        0.01334736419742129715,
#        0.01259400487133206984, #20
#        0.01191829593639203987,
#        0.01130897010592253677,
#        0.01075682530331795758,
#        0.01025427408185346821,
#        0.00979500577007117565,
#        0.00937372981918208152,
#        0.00898597850284337809,
#        0.00862795358070943465,
#        0.00829640592773924140,
#        0.00798854016260335483, #30
#        0.00770193843225976117,
#        0.00743449901783153060)
# su <- 0
# for (n in 1:32) {
#       su <- su + c[n] * x^((2*n)-1)
# }
# THIS WORKED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Now to investigate how many c_n values are needed for the pt3 inverse.
# x <- c(1:60)
# z <- 0.999
# a <- array(NA, dim = length(x))
# for (n in 1:length(x)) {
#       a[n] <- c[n] * z^((2*n)-1)
# }
# plot(x,c)
# plot(x,log(c))
# plot(x,a)
# plot(x,log(a))
