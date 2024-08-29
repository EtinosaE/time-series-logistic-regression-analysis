# Install and load necessary libraries
install.packages(c("tseries","tidyverse","forecast","ggplot2","smooth"))
library(tseries)# adf.test(train)
library(tidyverse)#subset
library(forecast)#for forcasting of 
library(gridExtra)#for the layout arrangement
library(ggplot2)#for plot
library(smooth)

# Set working directory
setwd("C:\\Users\\Admin\\Desktop\\stat_project_CA2")

# Load dataset
weather_revised <- read.csv("weather_revised.csv", header = TRUE, na.string = c(""), stringsAsFactors = TRUE)

##################### Data Exploration ######################

#view the data frame type amd format
str(weather_revised)
head(weather_revised)
summary(weather_revised)# look at data summary
dim(weather_revised) # check the dimension of the data: 29889, 9

###############data Cleaning and Preprocessing ##################

# Check for missing values and display the result
col_missing_counts <- colSums(is.na(weather_revised))
col_missing_counts

# in column: "gmin.Grass.Minimum.Temperature...degrees.C." 5 missing values and in column: "evap.Evaporation..mm." 2 missing values

# remove columns not needed for this time series analysis
cols_to_remove <- c("maxtp.Maximum.Air.Temperature...degrees.C." ,"mintp.Minimum.Air.Temperature...degrees.C." ,"gmin.Grass.Minimum.Temperature...degrees.C.",
                     "rain.Precipitation.Amount...mm.", "cbl..Mean.CBL.Pressure.hpa.", "pe.Potential.Evapotranspiration...mm.", "evap.Evaporation..mm.")
weather_revised <- weather_revised[, !colnames(weather_revised) %in% cols_to_remove]

# Print column names after removal
print(colnames(weather_revised))

# Convert the date column to Date type with the correct format
weather_revised$date <- as.Date(weather_revised$date, format="%d/%m/%Y")
print(weather_revised$date)# Check the result
str(weather_revised)#view the data frame agian 

#renaming the columns wdsp.Mean.Wind.Speed...knot.to wdsp
colnames(weather_revised)[colnames(weather_revised) == "wdsp.Mean.Wind.Speed...knot."] <- "wdsp"
summary(weather_revised)

# Create a list of variables you want to plot
var_plot <- c("date", "wdsp")
plots <- list()# Create a list to store the plots

# Loop through the variables and create plots
for (variable in var_plot) {
  plot <- ggplot(weather_revised, aes(x = .data[[variable]])) +
    geom_histogram(fill = "blue", bins = 30, color = "black") +
    labs(title = paste("Distribution of", variable), x = variable, y = "Frequency")
  
  plots[[variable]] <- plot
}

# Arrange and plot the plots in a grid with a 3x5 layout
grid.arrange(grobs = plots, ncol = 2, nrow = 2)

# check for outliers using box plot for visualizing the variables
variables <- c("date", "wdsp")
par(mfrow = c(1, 2))# Set up the layout for the plots

# Create box plots for each variable
for (variable in variables) {
  boxplot(weather_revised[[variable]], main = paste("Box Plot of", variable))
}
par(mfrow = c(1, 1))# Reset the layout to the normal

# correct outliers using IQR method
variables <- c("wdsp")

for (col in variables) {
  Q1 <- quantile(weather_revised[[col]], 0.25)
  Q3 <- quantile(weather_revised[[col]], 0.75)
  IQR <- Q3 - Q1
  k <- 1.5 # IQR multiplier 
  lower_bound <- Q1 - k * IQR
  upper_bound <- Q3 + k * IQR
  weather_revised <- weather_revised[weather_revised[[col]] >= lower_bound & weather_revised[[col]] <= upper_bound, ]
}
summary(weather_revised)

#box plot for visualizing the variables after outlier correction
variables <- c("date", "wdsp")
par(mfrow = c(1, 2))# Set up the layout for the plots

# Create box plots for each variable
for (variable in variables) {
  boxplot(weather_revised[[variable]], main = paste("Box Plot of", variable))
}
par(mfrow = c(1, 1))# Reset the layout to the normal

######################## Create Time Series #########################################

weatherts <- ts(
  weather_revised$wdsp,
  start = c(1942,1),
  end = c(2023,10),
  frequency = 12
)
train <- window(weatherts,start = c(2019,1), end = c(2022,12))
test <- window(weatherts,start = c(2023), end = c(2023, 10))
str(train)
str(test)
plot(time(train), train, type = "l", col = "black", lwd = 2, xlab = "Date", ylab = "Mean Wind Speed (knots)")
plot(time(test), test, type = "l", col = "black", lwd = 2, xlab = "Date", ylab = "Mean Wind Speed (knots)")

#Create visualizations to understand the nature and components of the raw time series
# Visualize by Plotting the Mean Wind Speed Over Time using autoplot
autoplot(train) +
  labs(title = "Mean Wind Speed Over Time",
       x = "Date",
       y = "Wind Speed")
par(mfrow = c(1, 1))# Reset the layout to the normal

########################## Simple model #######################################

#Fit average model
avg_model <- meanf(train, h = 12)

# Fit naive model
naive_model <- naive(train, h = 12)

# Fit seasonal model
seasonal_model <- snaive(train, h = 12)

# Fit drift model
drift_model <- rwf(train, h = 12, drift = TRUE)

# Print the forecasts
print(avg_model)
print(naive_model)
print(seasonal_model)
print(drift_model)

# Plot the forecasts
plot(avg_model, main = "Average Model")
plot(naive_model, main = "Naive Model")
plot(seasonal_model, main = "Seasonal Model")
plot(drift_model, main = "Drift Model")

# Print the accuracy of each model
accuracy(avg_model)
accuracy(naive_model)
accuracy(seasonal_model)
accuracy(drift_model)

# Create a data frame to compare models then evaluate with the best model
model_comparison <- data.frame(
  Model = c("avg_model", "naive_model", "seasonal_model", "drift_model"),
  RMSE = c(accuracy(avg_model)[2], accuracy(naive_model)[2], accuracy(seasonal_model)[2], accuracy(drift_model)[2]),
  MAE = c(accuracy(avg_model)[3], accuracy(naive_model)[3], accuracy(seasonal_model)[3], accuracy(drift_model)[3])
  )

# Print model comparison
print(model_comparison)

# evaluate with the avg_model model because it has the lowest RMSE(4.031094)
avg_model <- meanf(test, h = 12)

print(avg_model)
accuracy(avg_model)# ME     RMSE  MAE       MPE     MAPE MASE      ACF1
#Training set 3.553059e-16 3.606938 2.72 -24.17397 42.68041  NaN 0.2901614

########################## Exponential model #######################################

# Fit ses model
weatherwdsp_ses <- ses(train, h = 3)
round(accuracy(weatherwdsp_ses),3)

autoplot(weatherwdsp_ses)
autoplot(weatherwdsp_ses)+autolayer(fitted(weatherwdsp_ses),series = "Fitted")

# Fit holt model
weatherwdsp_holt <- holt(train, h = 3)
round(accuracy(weatherwdsp_holt),3)

autoplot(weatherwdsp_holt)
autoplot(weatherwdsp_holt)+autolayer(fitted(weatherwdsp_holt),series = "Fitted")

# Fit Holt-Winters model(triple)
weatherwdsp_hw <- hw(train, h = 3)
round(accuracy(weatherwdsp_hw),3)

autoplot(weatherwdsp_hw)
autoplot(weatherwdsp_hw)+autolayer(fitted(weatherwdsp_hw),series = "Fitted")

ets(train, model="ANN")
ets(train, model="AAN")
ets(train, model="AAA")

#comparing the model performance
model_comparison <- data.frame(
  Model = c("ses", "Holt", "Holt-Winters"),
  RMSE = c(accuracy(weatherwdsp_ses)[2],accuracy(weatherwdsp_holt)[2],accuracy(weatherwdsp_hw)[2]),
 MAE = c(accuracy(weatherwdsp_ses)[3],accuracy(weatherwdsp_holt)[3],accuracy(weatherwdsp_hw)[3]),
  MAPE = c(accuracy(weatherwdsp_ses)[5],accuracy(weatherwdsp_holt)[5],accuracy(weatherwdsp_hw)[5])
  )
print(model_comparison)

# evaluate 
weatherwdsp <- ets(test, model = "ZZZ")

print(weatherwdsp)
accuracy(weatherwdsp)

########################## ARIMA model #######################################

autoplot(train)

adf.test(train)

ndiffs(train)

deff <- train %>% diff()

ggtsdisplay(deff, main= "Differencing to Achieve Stationarity")



model <- Arima(train, order=c(1,1,0))
summary(model)

model2 <- Arima(train, order=c(1,0,0))
summary(model2)

model3 <- Arima(train, order=c(2,1,1))
summary(model3)

model4 <- Arima(train, order=c(2,1,0))
summary(model4)

model_auto <- auto.arima(train)
summary(model_auto)

# Comparing the model performance
model_comparison <- data.frame(
  Model = c("model", "model2", "model3", "model4"),
  RMSE = c(accuracy(model)[2], accuracy(model2)[2], accuracy(model3)[2], accuracy(model4)[2]),
  MAE = c(accuracy(model)[3], accuracy(model2)[3], accuracy(model3)[3], accuracy(model4)[3]),
  MAPE = c(accuracy(model)[5], accuracy(model2)[5], accuracy(model3)[5], accuracy(model4)[5])
  ) 
           

print(model_comparison)

# evaluate with the  fit411 model because it has the lowest RMSE(3.467209)and MAE(2.893804)
model2 <- Arima(test, order=c(1,0,0))
summary(model2)

print(model2)
accuracy(model2)

############################ Overall Comparing of the model ##################################

# overall Comparing the model performance using the best from simple models, exponential models and Arima models
model_comparison <- data.frame(
  Model = c("avg_model", "weatherwdsp_ses", "model2"),
  RMSE = c(accuracy(avg_model)[2], accuracy(weatherwdsp_ses)[2], accuracy(model2)[2]),
  MAE = c(accuracy(avg_model)[3], accuracy(weatherwdsp_ses)[3], accuracy(model2)[3]),
  MAPE = c(accuracy(avg_model)[5], accuracy(weatherwdsp_ses)[5], accuracy(model2)[5])
) 
print(model_comparison)

# Select the model with the lowest RMSE
optimal_model <- model_comparison[which.min(model_comparison$RMSE), ]

# Print the optimal model
cat("Optimal Model:", optimal_model$Model, "\n")
cat("Optimal Model RMSE:", optimal_model$RMSE, "\n")
