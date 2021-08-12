# Getting started
library(devtools)
install_github("LimpopoLab/hydrostats")
library(hydrostats)
library(dplyr)
library(ggplot2)
library(latex2exp)

# To update documentation:
devtools::document()
# https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html

x <- ((c(1:41))/10)-2.1
Erf <- erf(x)
Erfc <- 1-erf(x)
err <- data.frame(x,"Erf",y)

ggplot(err) +
      geom_line(aes(x = x, y = y)) +
      geom_line(aes(x = x, y = z)) +
      labs(x = "x", y = "y") +
      
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(aspect.ratio = 1) +
      theme(axis.text = element_text(face = "plain", size = 12))

# Formula from Wikipedia:
# ((pi^0.5)/2) * (z + pi*(z^3)/12 + 7*(pi^2)*(z^5)/480 + 127*(pi^3)*(z^7)/40320 + 4369*(pi^4)*(z^9)/5806080 + 34807*(pi^5)*(z^11)/182476800)
# but that doesn't work either.

# From Wolfram (which still doesn't work):
x <- erf(-2)
su <- 0
top <- 0
bottom <- 0
for (i in 1:21) {
      k <- i - 1
      top <- x^(2*k+1)
      bottom <- factorial(k) * (2*k+1)
      su <- su + (top / bottom)
}
t <- (2 / (pi^(0.5))) * su

