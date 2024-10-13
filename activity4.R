# install.packages(c("dplyr", "lubridate","ggplot2"))
library(lubridate)
library(dplyr)
library(ggplot2)

weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                    na.strings="#N/A")
weather$dateF <- mdy_hm(weather$Date)

# Removes the last and first element of date. Pairs consecutive elements from the two subvectors -> create interval in time
interval <- weather$dateF[-(length(weather$dateF))] %--% weather$dateF[-1]

# Interval but in a function type 
timeInterval <- function(x){
  x[-length(x)] %--% x[-1]}
timeInterval(weather$dateF)

# Prompt 1: Calculate a rolling average of air temperatures over eight 15 min measurements (2 hours) f
# or January of 2022 using a for loop. Make a plot of the 15 minute air temperature and the 
# rolling average.
weather$month <- month(weather$dateF)
weather$year <- year(weather$dateF)

Jan22 <- weather %>%
  filter(month ==1 & year == 2022)

rollAveTemp <- numeric() #empty numeric vector to store rolling average temperature values
for(i in 8:nrow(Jan22)){ #start from row 8 all the way to last row of Jan22 dataset
  rollAveTemp[i] <- mean(Jan22$AirTemp[(i-7):i])} # For each iteration, calculates the mean of air temperature over past 8 days
Jan22$rollAveTemp <- rollAveTemp

# Homework question 1
# Precipitation 
# Exclude any precipitations that occurs when the air temperature is below zero
# Exclude any precipitation measurement if X and Y levels are more than 2 degrees

weather$filtered_precipitation <- ifelse((weather$AirTemp < 0) |
                                           abs(weather$XLevel) > 2 |
                                           abs(weather$YLevel) > 2, 
                                         NA, 
                                         weather$Precip)

# Count the number of NA values in filtered_precipitation
na_count <- sum(is.na(weather$filtered_precipitation))
na_count


# Homework question 2
# Create a flag if the battery voltage falls below 8.5 volts  
# Code is right -> need to determine right variable for battery voltage
weather$volt_alert <- ifelse(weather$BatVolt <= 8.5, 1, 0)
alert_rows <- weather %>% filter(volt_alert == 1)
alert_rows

# Homework question 3
# Create a function that checks for observations that are in unrealistic data ranges in air temperature and solar radiation.
# Air temp range measurement specification from -50 to 60 degree celsius 
# Solar radiation range normally from to 0 - 1750 w/m^2

unrealisticObs <- function(x, range) {
  outlier_flags <- rep(NA, length(x))
  outlier_flags[!is.na(x) & (x < range[1] | x > range[2])] <- 1
  outlier_flags[!is.na(x) & (x >= range[1] & x <= range[2])] <- 0
  return(outlier_flags) 
}

weather$AirTemp_Outliers <- unrealisticObs(weather$AirTemp, range = c(-50, 60))
weather$SolRad_Outliers <- unrealisticObs(weather$SolRad, range = c(0, 1750))

summary(weather$AirTemp_Outliers)
summary(weather$SolRad_Outliers)

# Alternative method
unrealistic_z_score <- function(x) {
  mean <- mean(x, na.rm = TRUE)  
  sd <- sd(x, na.rm = TRUE)
  z_score <- (x - mean) / sd 
  outliers <- ifelse(abs(z_score) > 2, 1, 0)
  return(outliers)}

weather_z_outliers_air = unrealistic_z_score(weather$AirTemp)
weather_z_outliers_solar = unrealistic_z_score(weather$SolRad)

summary(weather_z_outliers_air)
summary(weather_z_outliers_solar)


# Homework question 4
# make a plot of winter air temperatures in Jan - Mar 21. Check for persistence issue that might indicate snow accumulation

JanMar21 <- weather %>%
  filter(year == 2021 & month >= 1 & month <= 3)

ggplot(JanMar21, aes(x = dateF, y = AirTemp, group = as.Date(dateF))) +
  geom_line() +
  labs(title = "Winter Air Temperatures (Jan - Mar 2021)",
       x = "Months",
       y = "Air Temperature (°C)") +
  theme_minimal()

# Homework question 5
# Total daily precipation in March and April of 2021
# use a for loop to convert any days that include temperature less than 35F on that day
# or the day prior 
# How many daily observations have precipitation observations (not a NA) in your final data table?

# This is an application of the rolling average problem conducted in prompt 1 (must use indexing)
MarApr21 <- weather %>%
  filter(year == 2021 & month >= 3 & month <= 4)

# Convert fahrenheit column
MarApr21$airFahrenheit <- MarApr21$AirTemp *(9/5) + 32

for (i in 2:nrow(MarApr21)) { 
  if (MarApr21$airFahrenheit[i] < 35 | MarApr21$airFahrenheit[i - 1] < 35) {
    MarApr21$filtered_precipitation[i] <- NA}}
sum(!is.na(MarApr21$filtered_precipitation))




