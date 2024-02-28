library(fable)
library(tidyverse)
library(ggplot2)
library(feasts)
library(dplyr)
library(tsibble)
library(ggfortify)
library(forecast)



#Read in Historical Bloom DOY Data
dc <- read.table('C:/Users/david/Desktop/GMU Files/2024 Spring/Capstone/washingtondc.csv', sep=',', header=TRUE)
liestal <- read.table('C:/Users/david/Desktop/GMU Files/2024 Spring/Capstone/liestal.csv', sep=',', header=TRUE)
kyoto <- read.table('C:/Users/david/Desktop/GMU Files/2024 Spring/Capstone/kyoto.csv', sep=',', header=TRUE)
vancouver <- read.table('C:/Users/david/Desktop/GMU Files/2024 Spring/Capstone/vancouver.csv', sep=',', header=TRUE)

predictions <- data.frame(location = character(), prediction = numeric(), lower = numeric(), upper = numeric())


##################################################
#KYOTO
##################################################

#Kyoto Average Temperatures by Month since 2010
kyoto_jan <- c(3,0,2,2,3,3,3,3,3,4,6,4,3,4)
kyoto_feb <- c(5,4,2,3,3,3,4,3,3,6,6,6,3,5)


#filter the overall data to create a dataframe of recent Kyoto bloom DOY data
k_recent <- kyoto[kyoto$year >= 2010,]


#Add average temperature to dataframe
k_recent$jan_avg <- kyoto_jan
k_recent$feb_avg <- kyoto_feb
k_recent

# Convert to a tsibble object
k_recent_tsbl <- as_tsibble(k_recent, index = year)

# Fit a dynamic regression model with ARIMA errors
model_k <- k_recent_tsbl %>%
  model(
    ARIMA(bloom_doy ~ year + feb_avg)
  )

# Create a new tsibble for the year 2024
new_data_k <- tibble(year = 2024, feb_avg = 6.6) %>%
  as_tsibble(index = year)

# Use the predict function with the new data
predicted_bloom_doy_k <- model_k %>%
  forecast(new_data_k)

# Extract the 90% prediction interval
pred_interval_k <- predicted_bloom_doy_k %>%
  hilo(level = 90)

# Convert the hilo object to a character string
hilo_as_char_k <- as.character(pred_interval_k[6]$`90%`)

# Use strsplit to split the string into two parts at the comma
split_char_k <- strsplit(hilo_as_char_k, ",")

# The lower part
lower_limit_k <- as.numeric(gsub("\\[", "", split_char_k[[1]][1]))

# The upper part
upper_limit_k <- as.numeric(gsub("\\]", "", split_char_k[[1]][2]))

#Add predictions to data frame
predictions[1,] <- c('Kyoto', round(predicted_bloom_doy_k$.mean, 0), round(as.numeric(lower_limit_k),0), round(as.numeric(upper_limit_k),0))



####################################################
#DC
####################################################


# DC Average Temperatures by Month since 2010
dc_jan <- c(0,-1,3,3,-2,0,0,4,1,2,5,3,0,6)
dc_feb <- c(-2,-1,5,2,1,-3,3,7,7,5,6,2,4,8)

# Filter the overall data to create a dataframe of recent DC bloom DOY data
dc_recent <- dc[dc$year >= 2010,]

# Add average temperature to dataframe
dc_recent$jan_avg <- dc_jan
dc_recent$feb_avg <- dc_feb

# Convert to a tsibble object
dc_recent_tsbl <- as_tsibble(dc_recent, index = year)

# Fit a dynamic regression model with ARIMA errors
model_dc <- dc_recent_tsbl %>%
  model(
    ARIMA(bloom_doy ~ year + feb_avg)
  )

# Create a new tsibble for the year 2024
new_data_dc <- tibble(year = 2024, feb_avg = 5.3) %>%
  as_tsibble(index = year)

# Use the predict function with the new data
predicted_bloom_doy_dc <- model_dc %>%
  forecast(new_data_dc)

# Extract the 90% prediction interval
pred_interval_dc <- predicted_bloom_doy_dc %>%
  hilo(level = 90)

# Convert the hilo object to a character string
hilo_as_char_dc <- as.character(pred_interval_dc[6]$`90%`)

# Use strsplit to split the string into two parts at the comma
split_char_dc <- strsplit(hilo_as_char_dc, ",")

# The lower limit
lower_limit_dc <- as.numeric(gsub("\\[", "", split_char_dc[[1]][1]))

# The upper limit
upper_limit_dc <- as.numeric(gsub("\\]", "", split_char_dc[[1]][2]))

#Add predictions to data frame
predictions[2,] <- c('DC', round(predicted_bloom_doy_dc$.mean, 0), round(as.numeric(lower_limit_dc),0), round(as.numeric(upper_limit_dc),0))




####################################################
#Liestal
####################################################


# Liestal Average Temperatures by Month since 2010
liestal_jan <- c(-3,0,2,0,3,2,2,-3,5,1,3,0,2)
liestal_feb <- c(0,2,-4,-1,3,0,4,4,0,5,6,4,4)

# Filter the overall data to create a dataframe of recent Liestal bloom DOY data
liestal_recent <- liestal[liestal$year >= 2011,]

# Add average temperature to dataframe
liestal_recent$jan_avg <- liestal_jan
liestal_recent$feb_avg <- liestal_feb

# Convert to a tsibble
liestal_recent_tsbl <- as_tsibble(liestal_recent, index = year)

# Fit a dynamic regression model with ARIMA errors
model_l <- liestal_recent_tsbl %>%
  model(
    ARIMA(bloom_doy ~ year + feb_avg)
  )

# Create a new tsibble for the year 2024
new_data_l <- tibble(year = 2024, feb_avg = 5.6) %>%
  as_tsibble(index = year)

# Use the predict function with the new data
predicted_bloom_doy_l <- model_l %>%
  forecast(new_data_l)

# Extract the 90% prediction interval
pred_interval_l <- predicted_bloom_doy_l %>%
  hilo(level = 90)

# Convert the hilo object to a character string
hilo_as_char_l <- as.character(pred_interval_l[6]$`90%`)

# Use strsplit to split the string into two parts at the comma
split_char_l <- strsplit(hilo_as_char_l, ",")

# The lower limit
lower_limit_l <- as.numeric(gsub("\\[", "", split_char_l[[1]][1]))

# The upper limit
upper_limit_l <- as.numeric(gsub("\\]", "", split_char_l[[1]][2]))

#Add predictions to data frame
predictions[3,] <- c('Liestal', round(predicted_bloom_doy_l$.mean, 0), round(as.numeric(lower_limit_l),0), round(as.numeric(upper_limit_l),0))




###############################################
#Vancouver and NYC
###############################################

all_recent <- rbind(k_recent, dc_recent, liestal_recent, vancouver_recent)
all_recent
# Define the variables of the linear model
formula <- bloom_doy ~ year + jan_avg + feb_avg + lat + long + alt

# Fit the linear model
set.seed(1)
lm_model <- lm(formula, data = all_recent)

# Print the model
summary(lm_model)

# Create a new data frame for NYC in the year 2024
new_data_nyc <- data.frame(year = 2024, jan_avg = 2, feb_avg = 0, lat = 40.73061, long = -73.93524, alt = 10)


# Make a prediction with a 90% prediction interval for NYC
predicted_bloom_doy_nyc <- predict(lm_model, newdata = new_data_nyc, interval = "prediction", level = 0.90)

predicted_bloom_doy_nyc[3]


#Add predictions to data frame
predictions[4,] <- c('NYC', round(predicted_bloom_doy_nyc[1], 0), round(predicted_bloom_doy_nyc[2],0), round(predicted_bloom_doy_nyc[3],0))


# Create a new data frame for Vancouver in the year 2024
new_data_v <- data.frame(year = 2024, jan_avg = 4, feb_avg = 1, lat = 49.22370, long = -123.163600, alt = 24)


# Make a prediction with a 90% prediction interval for Vancouver
predicted_bloom_doy_v <- predict(lm_model, newdata = new_data_v, interval = "prediction", level = 0.90)

predicted_bloom_doy_v[1]


#Add predictions to data frame
predictions[5,] <- c('Vancouver', round(predicted_bloom_doy_v[1], 0), round(predicted_bloom_doy_v[2],0), round(predicted_bloom_doy_v[3],0))



###############################################
#Plot Seasonal and Trend Data
###############################################


# Plot the seasonal component
autoplot(cherrydecompose$seasonal) +
  ggtitle("Seasonal Component")

# Plot the trend component
autoplot(cherrydecompose$trend) +
  ggtitle("Trend Component")



###############################################
#Create Predictions CSV
###############################################

#Check Predictions frame
predictions


#Write predictions
write.csv(predictions, file= 'C:/Users/david/Desktop/GMU Files/2024 Spring/Capstone/Predictions_Buxton.csv', row.names = FALSE)
