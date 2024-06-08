library(GGally)
library(ggplot2)  # For creating visualizations
library(dplyr)    # For data manipulation
library(tidyr)
library(plm)
library(psych)
library(forecast)
library(corrplot)
library(readxl)
library(readxl)
library(glmnet)
library(naniar)

data <- read_excel("data.xlsx")
#View(data)
attach(data)
MISSING_DATA <- read_excel("MISSING DATA.xlsx")


# Line plot for GDP over time
data$Year = as.factor(data$Year)
ggplot(data, aes(x = Year, y = GDP, group = Country, color = Country)) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "GDP Over Time",
       x = "Year",
       y = "GDP")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create a scatterplot with correlation value
attach(data)
ggplot(data, aes(x = PH, y = GDP))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  annotate("text", x = max(data$PH), y = max(data$GDP), 
           label = paste("Correlation:", round(cor(data$PH, data$GDP), 2)),
           hjust = 1, vjust = 1, size = 4, color = "blue") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Scatterplot of Poverty Headcount vs. GDP",
       x = "Poverty Headcount(% Population)",
       y = "GDP(US $)")

# Create a bar plot for Population Growth Rate by Country
ggplot(data, aes(x = Country, y = P, color = Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Population Growth Rate by Country",
       x = "Country",
       y = "Population Growth Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))


# Create a line plot for Education Enrollment over time
data$Year <- as.factor(data$Year)
ggplot(data, aes(x = Year, y = EEL,group = Country, color = Country)) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Education Enrollment Over Time",
       x = "Year",
       y = "Education Enrollment") 


# Calculate summary statistics
# Custom mode function
calculate_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
# Calculate summary statistics
summary_stats <- data.frame(
  Variable = colnames(data)[3:8],
  Mean = sapply(data[, 3:8], mean, na.rm = TRUE),
  Median = sapply(data[, 3:8], median, na.rm = TRUE),
  Mode = sapply(data[, 3:8], calculate_mode),
  SD = sapply(data[, 3:8], sd, na.rm = TRUE),
  Skewness = sapply(data[, 3:8], skew, na.rm = TRUE),
  Kurtosis = sapply(data[, 3:8], kurtosi, na.rm = TRUE)
)
summary_stats


# Calculate the correlation matrix
correlation_matrix <- cor(data[,c("GDP", "EEL", "PH", "UR", "P", "LE")])
correlation_matrix

# Calculate the correlation matrix
correlation_matrix <- cor(MISSING_DATA[,c("GDP", "EducationEnrolment", "PovertyHeadCount",
                                          "UnemploymentRate", "PopulationGrowthRate", "LifeExpectancy")],
                          use = "pairwise.complete.obs")
correlation_matrix

  # Hypothesis 1
# Create two vectors for life expectancy (LE) based on PH groups
le_below_average <- data$LE[data$PH < mean(data$PH)]
le_above_average <- data$LE[data$PH >= mean(data$PH)]

# Perform the two-sample t-test
t_test_result <- t.test(le_below_average, le_above_average)

# Print the t-test result
print(t_test_result)


# Hypothesis 2
# Calculate the mean GDP
mean_gdp <- mean(data$GDP)

# Use the mean as the threshold
threshold <- mean_gdp
# Check the value of the threshold
threshold
# Hypothesis 2: Is there a significant difference in EEL between high GDP and low GDP countries?
# Null Hypothesis (H0): There is no significant difference.
# Alternative Hypothesis (H1): There is a significant difference.

# Subset the data into high GDP and low GDP countries based on a threshold
high_gdp_countries <- data[data$GDP >= threshold, ]
low_gdp_countries <- data[data$GDP < threshold, ]

# Perform a two-sample t-test to compare EEL between the two groups
t_test_result <- t.test(high_gdp_countries$EEL, low_gdp_countries$EEL)

# Display the results
t_test_result

# POVERTY TIME SERIES PLOT
PH_time_series <- ts(data$PH, frequency = 1)

# Plot the Poverty Headcount(% Population) time series
plot(PH_time_series, main = "Poverty Headcount(% Population)
 Time Series", ylab = "Poverty Headcount")

#UNEMPLOYMENT TIME SERIES PLOT

UR_time_series <- ts(data$UR, frequency = 1)

# Plot the GDP time series
plot(UR_time_series, main = "Unemployment Rate
 Time Series", ylab = "Unemployment Rate")

# Time series plot for GDP

gdp_time_series <- ts(data$GDP, frequency = 1)

# Plot the GDP time series
plot(gdp_time_series, main = "GDP Time Series", ylab = "GDP")


MISSING_DATA <- read_excel("MISSING DATA.xlsx")


# Heatplot of missingness across the entire data frame 
vis_miss(MISSING_DATA) +
  ggtitle("Missing Values Visualization") +
  theme(plot.title = element_text(hjust = 0.5))


# Heatplot of missingness across the entire data frame after replacement
vis_miss(data) +
  ggtitle("Missing Values Visualization") +
  theme(plot.title = element_text(hjust = 0.5))

numeric_columns <- data[, c("GDP", "PH", "EEL", "UR", "P", "LE")]

# Calculate z-scores for each numeric column
z_scores <- scale(numeric_columns)

# Set the threshold for outlier detection (adjust as needed)
threshold <- 2

# Identify outliers based on the absolute z-scores exceeding the threshold
outliers <- abs(z_scores) > threshold

# Summarize the number of outliers in each column
outliers_summary <- colSums(outliers)

# Print the summary
print(outliers_summary)




# Boxplot to visualize distribution and outliers

boxplot(numeric_columns, col = ifelse(outliers, "red", "blue"), main = "Numeric Columns Boxplot", names = c("GDP", "PH", "EEL", "UR", "P", "LE"))
# Add a legend
legend("topright", legend = c("Normal", "Outlier"), fill = c("blue", "red"))


# Numeric columns
numeric_columns <- data[, c("GDP", "PH", "EEL", "UR", "P", "LE")]
# Numeric columns
numeric_columns <- c("GDP", "PH", "EEL", "UR", "P", "LE")
for (col_name in numeric_columns) {
  col_values <- data[[col_name]]
  
  # Calculate median and interquartile range
  median_val <- median(col_values, na.rm = TRUE)
  iqr <- IQR(col_values, na.rm = TRUE)
  
  # Define upper and lower bounds for outliers
  upper_bound <- median_val + 1.5 * iqr
  lower_bound <- median_val - 1.5 * iqr
  
  # Identify and replace outliers
  outliers <- col_values > upper_bound | col_values < lower_bound
  if (any(!is.na(outliers) & outliers)) {
    print(paste("Handling outliers in column:", col_name))
    
    # Print original median
    print(paste("Original median:", median_val))
    
    # Replace outliers with median
    data[outliers, col_name] <- as.numeric(median_val)
    
    # Print new median
    print(paste("New median:", median(data[[col_name]], na.rm = TRUE)))
  } else {
    print(paste("No outliers found in column:", col_name))
  }
}




# Fit a linear regression model
model <- lm(LE ~ GDP + EEL , data = data)

# Summary of the regression model
summary(model)


# Fit a multiple linear regression model
model_multiple <- lm(GDP ~ EEL + PH + UR + P + LE, data = data)

# Summary of the regression model
summary(model_multiple)


# Create a matrix of independent variables

X <- as.matrix(data[, c("EEL", "PH", "UR", "P", "LE")])

# Create a vector of the dependent variable
Y <- data$GDP

# Fit a Ridge Regression model
ridge_model <- glmnet(X, Y, alpha=0)  # Alpha=0 specifies Ridge Regression

# Plot the cross-validated mean squared error (MSE) as a function of lambda
plot(ridge_model)

# Choose the lambda with the minimum cross-validated MSE
best_lambda <- ridge_model$lambda.min

# Refit the model with the best lambda
best_ridge_model <- glmnet(X, Y, alpha=0, lambda=best_lambda)

# To get the coefficients
coefficients(best_ridge_model)


library(glmnet)

# Matrix setup: Create a matrix of independent variables (socio-economic indicators) and the dependent variable (GDP).


X <- as.matrix(data[, c("EEL", "PH", "UR", "P", "LE")])

y <- data$GDP

# Lasso Model: Build a Lasso Regression model using the "glmnet" function.
lasso_model <- glmnet(X, y, alpha = 1)  # Setting alpha to 1 indicates Lasso regression.

# Cross-Validation: Perform k-fold cross-validation (e.g., 10-fold) to select the optimal lambda value.
cv_model <- cv.glmnet(X, y, alpha = 1)  # Setting alpha to 1 for Lasso regression.

# Find the optimal lambda value with minimum mean squared error (MSE):
optimal_lambda <- cv_model$lambda.min

# Fit Lasso Model: Fit the Lasso Regression model using the optimal lambda.
lasso_fit <- glmnet(X, y, alpha = 1, lambda = optimal_lambda)

# View Coefficients: Use the "coef" function to view the coefficients of the Lasso model.
lasso_coefficients <- coef(lasso_fit)

# Interpret Coefficients: Interpret the coefficients to understand which socio-economic indicators are most influential.

# Print the coefficients to view their values.
print(lasso_coefficients)


#ARIMA MODEL

time_series_data <- ts(data$GDP, frequency = 1)

# Fit an ARIMA model
arima_model <- auto.arima(time_series_data)

# Print the summary of the ARIMA model
summary(arima_model)

# Plot the forecasts
plot(forecast(arima_model))


#VAR

data$Year <- as.numeric(data$Year)


# Ensure the data is in a time series format (assuming yearly data)
ts_data <- ts(data[, c("GDP", "PH", "UR")], start = min(data$Year), frequency = 1)  # Assuming yearly data

# Load the 'vars' package
library(vars)

# Fit the VAR model
var_model <- VAR(ts_data, p = 2)  

# View the model summary
summary(var_model)











