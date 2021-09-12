# Maximum precipitation statistical analysis

# Load needed libraries, includes custom package
library(readr)
library(dplyr)
library(lubridate)
library(devtools)
install_github("LimpopoLab/hydrostats", force = TRUE)
library(hydrostats)
library(ggplot2)

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

## First, use the lognormal distribution to determine the 