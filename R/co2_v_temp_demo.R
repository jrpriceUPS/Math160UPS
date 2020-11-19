#' Polar Demo
#'
#' Runs everything in the polar activity to make sure it works
#' @export
co2_v_temp_demo = function(){
#
# stats_co2_v_temp.R
#
# By Penny Rowe, Anoushka Adhav, Jacob Price, and Lea Fortmann
#
# Copyright 2020 by Penny Rowe and NorthWest Research Associates.
# You may use freely with proper attribution.
#
# Acknowledgments:
#
# Ice core temperature data comes from:
# https://www1.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/epica_domec/edc3deuttemp2007.txt",
# Jouzel, J., et al.  2007.
# EPICA Dome C Ice Core 800KYr Deuterium Data and Temperature Estimates.
# IGBP PAGES/World Data Center for Paleoclimatology
# Data Contribution Series # 2007-091.
# NOAA/NCDC Paleoclimatology Program, Boulder CO, USA.
#
# Ice core CO2 data comes from:
# https://www1.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/epica_domec/edc-co2-2008.txt"
# Luthi, D., M. Le Floch, B. Bereiter, T. Blunier, J.-M. Barnola,
# U. Siegenthaler, D. Raynaud, J. Jouzel, H. Fischer, K. Kawamura,
# and T.F. Stocker.  2008.
# High-resolution carbon dioxide concentration record 650,000-800,000
# years before present.
# Nature, Vol. 453, pp. 379-382, 15 May 2008.  doi:10.1038/nature06949

# .. Dependencies ...
#install.packages("devtools")
#install_github("jrpriceUPS/Math160UPS")
#    You need car and carData for scatterplot?
#    You need "readr" package to be able to run read_delim
#install.packages("car")
#install.packages("carData")
#install.packages("readr")

#library(car)
#library(carData)
#library("readr")

# Only read in individual commands with
#car::qqp()
#readr::read_delim()
#
#
#
# .. Notes
#    to use read_table, you may need to install readr as follows
#  > install.packages("readr")
#  > library("readr")
#

# install.packages("devtools")
# install_github("jrpriceUPS/Math160UPS")
library("devtools")
library("Math160UPS")

# .. Bring in functions
#source("co2_v_temp_rsrc.R")


# STUDENTS START HERE

# .. Read in the ice core CO2 data from the NOAA web site
#    the result is a tibble file, which is like a fancy dataframe
#    If you have any trouble, reload the library and move on
#website_ice_core_co2 = "https://www1.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/epica_domec/edc-co2-2008.txt"
#ice_core_co2 <- readr::read_table(website_ice_core_co2,skip = 773,
#                                  col_types = "dd")
data("ice_core_co2")

# .. Read in the ice core temperature data from the NOAA web site
#    the result is a tibble file, which is like a fancy dataframe
#    If you have any trouble, reload the library and move on
#website_ice_core_T = "https://www1.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/epica_domec/edc3deuttemp2007.txt"
#ice_core_T <- readr::read_table(website_ice_core_T,
#                                skip = 113,
#                                col_names = c('Bag', 'ztop', 'Age', 'Deuterium', 'Temperature'),
#                                col_types = "ddddd")
data("ice_core_T")

# # # # # #     MODERN polar Temperature    # # # # # # #

# .. You already have modern_polar_T. Check it out
#View(modern_polar_T)
data("modern_polar_T")

# .. Show the average temperature,
modern_polar_T$TAVG

# .. Make a histogram of temperatures
hist(modern_polar_T$TAVG, xlab='Temperature (Celsius)',
     ylab='Frequency of Temperature',
     main='Monthly Average Temperatures at Utqiagvik, AK from 1902-2020',
     col="turquoise", breaks=10)

# .. Get the mean, standard deviation, and variance for the average temperature
mean(modern_polar_T$TAVG)
mean(modern_polar_T$TAVG, na.rm = TRUE)
sd(modern_polar_T$TAVG, na.rm = TRUE)
var(modern_polar_T$TAVG, na.rm = TRUE)

# .. Get the min and max
min(modern_polar_T$TAVG, na.rm=TRUE)
max(modern_polar_T$TAVG, na.rm=TRUE)

# .. Get the range
range(modern_polar_T$TAVG, na.rm = TRUE)

# WHAT DO YOU THINK THE FOLLOWING LINES OF CODE DO? RUN TO FIND OUT.
# .. Get the years corresponding to the min and max average temperature
modern_polar_T$DATE[which.min(modern_polar_T$TAVG)]
modern_polar_T$DATE[which.max(modern_polar_T$TAVG)]

# .. Plot the annual average temperature with time
plot(modern_polar_T$DATE, modern_polar_T$TAVG, ylab='Average Temperature (Celsius)',
     xlab='Year', main='Utqiagvik, AK, Monthly Average Temperature, 1902-2020')




# # # # # #     MODERN CO2    # # # # # # #
data(modern_co2_backup)
#modern_co2 = modern_co2_backup
#View(modern_co2)

# .. What are the columns and how are they related? What's the best way to plot
#    the data?
plot(modern_co2_backup$year, modern_co2_backup$co2, xlab="Year", ylab = "CO2 (ppm)",
     main="Atmospheric CO2 Concentration from 1958-2020")
plot(modern_co2_backup$year, modern_co2_backup$co2, xlab="Year", ylab = "CO2 (ppm)",
     main="Atmospheric CO2 Concentration from 1958-2020", type = "l")
plot(modern_co2_backup$decimal_date, modern_co2_backup$co2, xlab="Year", ylab = "CO2 (ppm)",
     main="Atmospheric CO2 Concentration from 1958-2020", col="red", type = "l")


# # # # # #     Temperature vs CO2 in modern times    # # # # # # #

# .. Get T and co2 on the same year grid
modern_polar <- get_modern_polar_T_and_co2_on_same_grid(modern_polar_T, modern_co2_backup)

# .. Plot CO2 and T concentration as a function of year
plot_timeseries_modern_co2_and_T(modern_polar)

# .. Now plot CO2 v polar T
plot(modern_polar$co2, modern_polar$T, xlab = 'CO2 (ppm)',
     ylab = 'Temperature (C)', main = 'Temperature vs CO2 at Utqiagvik, Alaska')

# .. Get the correlation coefficient
cor(modern_polar$co2, modern_polar$T)
cor(modern_polar$co2, modern_polar$T, use = "complete.obs")


# .. It turns out to be more convenient to work with the temperature
#    anomaly, or the difference in temperature from some time period.
#    That allows you to compare temperature trends in different locations
#    Here we plot the anomaly. Note that the plot looks the same; only
#    the y tick labels have changed
plot(modern_polar$co2, modern_polar$T_anomaly,
     xlab = 'CO2 (ppm)',  ylab = 'Temperature Anomaly (C)',
     main = 'Temperature Anomaly at Utqiagvik, Alaska')

# .. The correlation coefficient is the same too.
cor(modern_polar$co2, modern_polar$T_anomaly, use = "complete.obs")


# Linear regression
#linreg()
#modern$co2
#modern$polar_T_anomaly

# .. Use the linear model (lm) function
relation <- lm(modern_polar$T_anomaly ~ modern_polar$co2)
print(relation)
print(summary(relation))
# abline(relation, cex = 1.3, pch = 16, xlab = " ", ylab = " ")

# .. Find the best fit line, add it to the model, and to the plot
model = lm(modern_polar$T_anomaly~modern_polar$co2)
model
abline(model)

# Check out the residuals
#find_residuals() # modern_polar$co2 modern_polar$T_anomaly

## .. Look at the histogram of standard deviations
#hist(rstandard(relation))

# .. Plot temperature anomaly and CO2from the ice core record
ice_core = get_ice_core_T_and_co2_on_same_grid(ice_core_co2, ice_core_T)

# FIX THE YEAR PENNY!!!!
plot_timeseries_ice_core_co2_and_T(ice_core_co2, ice_core_T)

plot(ice_core$co2, ice_core$temperature, col = 'blue',
     main = 'Temperature Anomaly',
     xlab = 'CO2 (ppm)', ylab = 'Temperature Anomaly (C)')

cor(ice_core$co2, ice_core$temperature, use = "complete.obs")

# run linreg
#linreg()
#ice_core$co
#ice_core$temperature

#relation <- lm(ice_core$temperature~ice_core$co2)
#print(relation)
#print(summary(relation))

plot(ice_core$co2, ice_core$temperature,
     col = 'blue', main = 'Temperature Anomaly',
     xlab = 'CO2 (ppm)', ylab = 'Temperature Anomaly (C)')

# .. Add T anomaly vs CO2 for modern times:
#    most recent ice core, polar, and global
add_modern_to_plot(ice_core, modern_polar)



}

