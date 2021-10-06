# Maximum precipitation statistical analysis

# solution, example at: https://raw.githubusercontent.com/LimpopoLab/hydrostats/main/precip_evi.R

# Load needed libraries, includes custom package
library(readr)
library(dplyr)
library(lubridate)
library(devtools)
install_github("LimpopoLab/hydrostats", force = TRUE)
library(hydrostats)
library(ggplot2)
library(latex2exp)

# Precipitation in mm, data from NOAA: ncdc.noaa.gov
x <- read_csv("nycprecip.csv")
# Station: USW00094728, NY CITY CENTRAL PARK, NY US
# 40.77898N, 73.96925W, 13m elevation
# Other stations:
# dat <- read_csv("https://duq.box.com/shared/static/iiz4bazn39ej3rire12sp1knf1xacrcy.csv")
# Stations:
# Laguardia   Boston, MA  Central Park JFK, NYC
# USW00014732 USW00014739 USW00094728  USW00094789

# x <- dat %>%
#       filter(STATION == "USW00094728") %>%
#       select(DATE,PRCP,TMIN,TMAX)

for (i in 1:nrow(x)) {
      x$hydro.yr[i] <- hyd.yr(x$DATE[i])
}

y <- x %>%
      group_by(hydro.yr) %>%
      summarize(ann.max = max(PRCP, na.rm = TRUE)) # annual maximum daily precip

ggplot(y, aes(x=hydro.yr, y=ann.max)) +
      geom_point() + geom_line(size=0.1) +
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(aspect.ratio = 1) +
      theme(axis.text = element_text(face = "plain", size = 12)) +
      xlab("Year") +
      ylab("Maximum Annual Daily Precipitation (mm)")

ggplot(y, aes(x=ann.max)) +
      geom_histogram(binwidth = 15, fill = "steelblue") +
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(aspect.ratio = 1) +
      theme(axis.text = element_text(face = "plain", size = 12)) +
      xlab("Maximum Annual Daily Precipitation (mm)") +
      ylab("# of Days")

## First, use the lognormal distribution to determine the 50- and 200-year precipitation levels.
# for the lognormal distribution, you first must identify the statistics of the log-transformed data:
x_log <- log10(y$ann.max)

df <- data.frame(x_log)
ggplot(df, aes(x=x_log)) +
      geom_histogram(binwidth = 0.1, fill = "steelblue") +
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(aspect.ratio = 1) +
      theme(axis.text = element_text(face = "plain", size = 12)) +
      xlab(TeX('$log_{10} ($Maximum Annual Daily Precipitation (mm)$)$')) +
      ylab("# of Days")

m_log <- mean(x_log)
s_log <- stdev(x_log)
Tr <- c(50, 200)
p <- 1 - (1/Tr)
v <- qnorm(p, mean = m_log, sd = s_log) # inverse normal disribution
precip <- 10^v

ggplot(y, aes(x=ann.max)) +
      geom_histogram(binwidth = 15, fill = "steelblue") +
      geom_vline(xintercept = precip[1]) +
      geom_vline(xintercept = precip[2]) +
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(aspect.ratio = 1) +
      theme(axis.text = element_text(face = "plain", size = 12)) +
      xlab("Maximum Annual Daily Precipitation (mm)") +
      ylab("# of Days")

## Second, use the Extreme Value Distribution type I to determine the 50- and 200-year precipitation levels.
precip <- evi(y$ann.max, p)

ggplot(y, aes(x=ann.max)) +
      geom_histogram(binwidth = 15, fill = "steelblue") +
      geom_vline(xintercept = precip[1]) +
      geom_vline(xintercept = precip[2]) +
      theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(aspect.ratio = 1) +
      theme(axis.text = element_text(face = "plain", size = 12)) +
      xlab("Maximum Annual Daily Precipitation (mm)") +
      ylab("# of Days")

## Third, determine the return period of the precipitation that resulted from hurricane Ida on 01 Sep 2021?
dt <- ymd("2021-09-01")
i <- which(x$DATE==dt)
p <- x$PRCP[i]
Fx <- pevi(y$ann.max, p)
Tr <- 1 / (1-Fx)

## Also, what happened on 21 August 2021 - a little more than a week before Ida?
dt <- ymd("2021-08-21")
i <- which(x$DATE==dt)
p <- x$PRCP[i]
Fx <- pevi(y$ann.max, p)
Tr <- 1 / (1-Fx)

# Ida was certainly worse, over one-and-one-half times the precipitation, but that's a lot of water.
