library(GGally)
library(ggplot2)  # For creating visualizations
library(dplyr)    # For data manipulation
library(tidyr)
library(plm)
library(psych)
library(forecast)
library(corrplot)
library(readxl)


my_data <- read_excel("CD.xlsx")

data=my_data

missing_counts <- colSums(is.na(my_data))
print(missing_counts)
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

# Identify numeric columns
numeric_columns <- sapply(MISSING_CD, is.numeric)

# Impute missing values with the mean for numeric columns
for (col in colnames(MISSING_CD[, numeric_columns])) {
  MISSING_CD[, col][is.na(MISSING_CD[, col])] <- mean(MISSING_CD[, col], na.rm = TRUE)
}




#Correlation analysis
# Calculate the correlation matrix
correlation_matrix <- cor(my_data[, c("GDP", "EEL", "PH", "UR", "P", "LE")],use = "complete.obs")
correlation_matrix

# Linear Regression
lm_model <- lm(LE ~ GDP + EEL, data = my_data)
# Summary of the Linear Regression Model
summary(lm_model)

# Perform Multiple Regression
multiple_lm_model <- lm(GDP ~  + EEL + PH + UR + P + LE, data = my_data)

# Summary of the Multiple Regression Model
summary(multiple_lm_model)

# LOGISTICE REGRESSION
# Create a binary variable
my_data$PH_binary <- ifelse(my_data$PH > 1.0, 1, 0)

# Logistic Regression
logistic_model <- glm(PH_binary ~ GDP + EEL, data = my_data, family = binomial)

# Summary of the Logistic Regression Model
summary(logistic_model)







# Create a correlation plot with a heatmap
corrplot(correlation_matrix, method = "color")

# Print the correlation matrix
print(correlation_matrix)


# Create two vectors for life expectancy (LE) based on PH groups
le_below_average <- my_data$LE[my_data$PH < mean(my_data$PH)]
le_above_average <- my_data$LE[my_data$PH >= mean(my_data$PH)]

# Perform the two-sample t-test
t_test_result <- t.test(le_below_average, le_above_average)

# Print the t-test result
print(t_test_result)


# Calculate the median GDP
median_gdp <- median(my_data$GDP)

# Use the median as the threshold
threshold <- median_gdp

# Check the value of the threshold
threshold

# Hypothesis 2: Is there a significant difference in EEL between high GDP and low GDP countries?
# Null Hypothesis (H0): There is no significant difference.
# Alternative Hypothesis (H1): There is a significant difference.

# Subset the data into high GDP and low GDP countries based on a threshold
high_gdp_countries <- my_data[my_data$GDP >= threshold, ]
low_gdp_countries <- my_data[my_data$GDP < threshold, ]

# Perform a two-sample t-test to compare EEL between the two groups
t_test_result <- t.test(high_gdp_countries$EEL, low_gdp_countries$EEL)

# Display the results
t_test_result

# Load necessary libraries (if not already loaded)
# install.packages("dplyr")  # Uncomment and run if you haven't installed the library





#TIME SERIE PLOT
# Load necessary libraries


gdp_time_series <- ts(my_data$GDP, frequency = 1)

# Plot the GDP time series
plot(gdp_time_series, main = "GDP Time Series", ylab = "GDP")

# POVERTY TIME SERIES PLOT
PH_time_series <- ts(my_data$PH, frequency = 1)

# Plot the Poverty Headcount(% Population) time series
plot(PH_time_series, main = "Poverty Headcount(% Population)
 Time Series", ylab = "Poverty Headcount")

#UNEMPLOYMENT TIME SERIES PLOT

UR_time_series <- ts(my_data$UR, frequency = 1)

# Plot the GDP time series
plot(UR_time_series, main = "Unemployment Rate
 Time Series", ylab = "Unemployment Rate")


# Line plot for GDP over time
my_data$Year = as.factor(my_data$Year)
ggplot(my_data, aes(x = Year, y = GDP, group = Country, color = Country)) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "GDP Over Time",
       x = "Year",
       y = "GDP")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter plot for GDP vs. Life Expectancy
ggplot(my_data, aes(x = GDP, y = LE)) +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "GDP vs. Life Expectancy",
       x = "GDP",
       y = "Life Expectancy")

# Histogram for Unemployment Rate
ggplot(my_data, aes(x = UR)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Distribution of Unemployment Rate",
       x = "Unemployment Rate",
       y = "Frequency")

# Create a line plot for Life Expectancy over time
ggplot(my_data, aes(x = Year, y = LE,group = Country, color = Country)) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Life Expectancy Over Time",
       x = "Year",
       y = "Life Expectancy") 

# Create a bar plot for Population Growth Rate by Country
ggplot(my_data, aes(x = Country, y = P, color = Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Population Growth Rate by Country",
       x = "Country",
       y = "Population Growth Rate") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

# Create a box plot for Unemployment Rate by Country
ggplot(my_data, aes(x = Country, y = UR, color = Country)) +
  geom_boxplot() +
  labs(title = "Unemployment Rate by Country",
       x = "Country",
       y = "Unemployment Rate") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create a line plot for Education Enrollment over time
my_data$Year <- as.factor(my_data$Year)
ggplot(my_data, aes(x = Year, y = EEL,group = Country, color = Country)) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Education Enrollment Over Time",
       x = "Year",
       y = "Education Enrollment") 

 
 



# Pair plot for multiple variables
my_data %>%
  select(GDP, EEL, PH, UR, LE) %>%
  ggpairs() +
  ggtitle("Pair Plot of Multiple Variables") +
  theme(plot.title = element_text(hjust = 0.5))


# Time series plot for Population
ggplot(my_data, aes(x = Year, y = P, group = 1)) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Population Over Time",
       x = "Year",
       y = "Population")

# Correlation matrix

data=(my_data[, c("GDP", "EEL", "PH", "UR", "LE")])
ggcorr(data, method = c("everything", "pearson")) 

# Compute the correlation matrix
correlation_matrix <- cor(data, method = "pearson")

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








# Heatmap for the correlation matrix
ggplot(data = data.frame(x = colnames(cor_matrix))) +
  geom_tile(aes(x = x, y = colnames(cor_matrix), fill = cor_matrix)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Correlation Heatmap")

# Boxplot of Life Expectancy by Country
ggplot(my_data, aes(x = Country, y = LE, color = Country)) +
  geom_boxplot() +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Life Expectancy by Country",
       x = "Country",
       y = "Life Expectancy")

# Boxplot of GDP by Year
ggplot(my_data, aes(x = as.factor(Year), y = GDP, color = Country)) +
  geom_boxplot() +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "GDP by Year",
       x = "Year",
       y = "GDP")
# Create a scatter plot for Poverty Rate vs. GDP
ggplot(my_data, aes(x = GDP, y = PH, color = "blue")) +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Poverty Rate vs. GDP",
       x = "GDP",
       y = "Poverty Rate") 





# Perform a simple linear regression
model <- lm(LE ~ GDP, data = my_data)

# Check the summary of the regression model
summary(model)

# Hypothesis testing for GDP's effect on life expectancy
# Null Hypothesis (H0): There is no association between GDP and life expectancy
# Alternative Hypothesis (H1): There is an association between GDP and life expectancy

# Perform a hypothesis test using the F-statistic
test_result <- anova(model)
p_value <- test_result$"Pr(>F)"[1]

# Set the significance level (alpha)
alpha <- 0.05

# Check if the p-value is less than the significance level
if (p_value < alpha) {
  cat("Reject the null hypothesis: There is a statistically significant association between GDP and life expectancy.")
} else {
  cat("Fail to reject the null hypothesis: There is no statistically significant association between GDP and life expectancy.")
}

# Visualize the regression line and scatter plot
ggplot(my_data, aes(x = GDP, y = LE,color = "blue")) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Regression of GDP on Life Expectancy",
       x = "GDP",
       y = "Life Expectancy")




  # For panel data analysis

# Create a panel data model with fixed-effects
fixed_effects_model <- plm(LE ~ GDP + EEL + PH + UR + P, data = my_data, model = "within")

# Summarize the fixed-effects model
summary(fixed_effects_model)


# Create a panel data model with random-effects
random_effects_model <- plm(LE ~ GDP + EEL + PH + UR + P, data = my_data, model = "random")

# Summarize the random-effects model
summary(random_effects_model)




#Prediction
# Fit a multiple linear regression model
model <- lm(GDP ~ EEL + PH + UR + P + LE, data = my_data)

# Summary of the regression model
summary(model)

# Make predictions based on the model
predicted_gdp <- predict(model, newdata = my_data)

# Add the predicted GDP values to the original dataset
my_data$Predicted_GDP <- predicted_gdp

# View the updated dataset with predicted GDP values
View(my_data)


