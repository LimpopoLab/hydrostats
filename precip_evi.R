# Maximum precipitation statistical analysis

# Load needed libraries, includes custom package
library(readr)
library(dplyr)
library(lubridate)
library(devtools)
install_github("LimpopoLab/hydrostats", force = TRUE)
library(hydrostats)

# Precipitation in mm, data from NOAA: ncdc.noaa.gov
dat <- read_csv("")
# Station: USW00094728, NY CITY CENTRAL PARK, NY US
# 40.77898N, 73.96925W, 13m elevation
# Other stations:
#dat <- read_csv("https://duq.box.com/shared/static/iiz4bazn39ej3rire12sp1knf1xacrcy.csv")
# Stations:
# Laguardia   Boston, MA  Central Park JFK, NYC
# USW00014732 USW00014739 USW00094728  USW00094789
      
x <- dat %>%
      filter(STATION == "USW00094728") %>%
      select(DATE,PRCP,TMIN,TMAX)

for (i in 1:nrow(x)) {
      x$hydro.yr[i] <- hyd.yr(x$DATE[i])
}
      
y <- x %>%
      group_by(hydro.yr) %>%
      summarize(ann.max = max(PRCP)) # annual maximum daily precip


