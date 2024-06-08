# Given data
Evave <- 129       # Population mean
Obsave <- 121.3    # Sample mean
SD <- 47           # Population standard deviation
n <- 247           # Sample size

# Calculate the standard error of the mean (SEave)
SEave <- SD / sqrt(n)

# Calculate the Z-test statistic
Z <- (Obsave - Evave) / SEave

# Calculate the p-value
p_value <- pnorm(Z)

# Print the results
cat("Test Statistic (Z):", Z, "\n")
cat("P-Value:", p_value, "\n")

# Compare p-value to significance level (e.g., 0.05)
alpha <- 0.05
if (p_value < alpha) {
  cat("Reject the null hypothesis. There is evidence that heavy internet users watch less TV than the typical American.\n")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to conclude that heavy internet users watch less TV than the typical American.\n")
}
print(p_value)
print(z)


# Given data
sample_size <- 1000
observed_successes <- 430
null_proportion <- 0.5

# Calculate the sample proportion
sample_proportion <- observed_successes / sample_size

# Calculate the standard error
standard_error <- sqrt((null_proportion * (1 - null_proportion)) / sample_size)

# Calculate the z-score
z_score <- (sample_proportion - null_proportion) / standard_error

# Calculate the p-value
p_value <- pnorm(z_score)

# Print the results
cat("Sample Proportion:", sample_proportion, "\n")
cat("Z-Score:", z_score, "\n")
cat("P-Value:", p_value, "\n")

# Compare p-value to significance level (e.g., 0.05)
alpha <- 0.05
if (p_value < alpha) {
  cat("Reject the null hypothesis. There is significant evidence that fewer than half of adult Americans can name at least one currently serving Supreme Court justice.\n")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to conclude that fewer than half of adult Americans can name at least one currently serving Supreme Court justice.\n")
}


# Given data
sample_mean <- 90
population_mean_null <- 95
population_sd <- 30
sample_size <- 50

# Calculate the standard error
standard_error <- population_sd / sqrt(sample_size)

# Calculate the t-statistic
t_statistic <- (sample_mean - population_mean_null) / standard_error

# Calculate the degrees of freedom for a one-sample t-test
degrees_of_freedom <- sample_size - 1

# Calculate the p-value
p_value <- pt(t_statistic, df = degrees_of_freedom)

# Print the results
cat("Sample Mean:", sample_mean, "\n")
cat("T-Statistic:", t_statistic, "\n")
cat("P-Value:", p_value, "\n")

# Compare p-value to significance level (e.g., 0.05)
alpha <- 0.05
if (p_value < alpha) {
  cat("Reject the null hypothesis. There is evidence that the new system has reduced wait times at this restaurant.\n")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to conclude that the new system has reduced wait times at this restaurant.\n")
}


# Given data
sample_size <- 2205
observed_failures <- 750
null_proportion <- 1/3  # 0.333

# Calculate the sample proportion
sample_proportion <- observed_failures / sample_size

# Calculate the standard error
standard_error <- sqrt((null_proportion * (1 - null_proportion)) / sample_size)

# Calculate the z-score
z_score <- (sample_proportion - null_proportion) / standard_error

# Calculate the p-value
p_value <- 1 - pnorm(z_score)

# Print the results
cat("Sample Proportion:", sample_proportion, "\n")
cat("Z-Score:", z_score, "\n")
cat("P-Value:", p_value, "\n")

# Compare p-value to significance level (e.g., 0.05)
alpha <- 0.05
if (p_value < alpha) {
  cat("Reject the null hypothesis. There is evidence that more than a third of adolescents have a low level of cardiovascular fitness.\n")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to conclude that more than a third of adolescents have a low level of cardiovascular fitness.\n")
}



# Given data
sample_mean <- 594
population_mean_null <- 612
population_sd <- 100
sample_size <- 100

# Calculate the standard error
standard_error <- population_sd / sqrt(sample_size)

# Calculate the t-statistic
t_statistic <- (sample_mean - population_mean_null) / standard_error

# Calculate the degrees of freedom for a one-sample t-test
degrees_of_freedom <- sample_size - 1

# Calculate the p-value
p_value <- pt(t_statistic, df = degrees_of_freedom)

# Print the results
cat("Sample Mean:", sample_mean, "\n")
cat("T-Statistic:", t_statistic, "\n")
cat("P-Value:", p_value, "\n")

# Compare p-value to significance level (e.g., 0.05)
alpha <- 0.05
if (p_value < alpha) {
  cat("Reject the null hypothesis. There is evidence that entering students' verbal abilities are declining.\n")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to conclude that entering students' verbal abilities are declining.\n")
}



# Given data
sample_proportion <- 196 / 300
null_proportion <- 0.55
sample_size <- 300

# Calculate the standard error
standard_error <- sqrt((null_proportion * (1 - null_proportion)) / sample_size)

# Calculate the z-score
z_score <- (sample_proportion - null_proportion) / standard_error

# Calculate the two-tailed p-value
p_value <- 2 * (1 - pnorm(abs(z_score)))

# Print the results
cat("Sample Proportion:", sample_proportion, "\n")
cat("Z-Score:", z_score, "\n")
cat("P-Value:", p_value, "\n")

# Compare p-value to significance level (e.g., 0.05)
alpha <- 0.05
if (p_value < alpha) {
  cat("Reject the null hypothesis. There is evidence that the proportion of people with the gene is different from the general population.\n")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to conclude that the proportion of people with the gene is different from the general population.\n")
}



# Given data
sample_proportion <- 430 / 1000
null_proportion <- 0.5
sample_size <- 1000

# Calculate the standard error
standard_error <- sqrt((null_proportion * (1 - null_proportion)) / sample_size)

# Calculate the z-score
z_score <- (sample_proportion - null_proportion) / standard_error

# Calculate the p-value
p_value <- pnorm(z_score)

# Print the results
cat("Sample Proportion:", sample_proportion, "\n")
cat("Z-Score:", z_score, "\n")
cat("P-Value:", p_value, "\n")

# Compare p-value to significance level (e.g., 0.05)
alpha <- 0.05
if (p_value < alpha) {
  cat("Reject the null hypothesis. There is evidence that fewer than half of adult Americans can name at least one currently serving Supreme Court justice.\n")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to conclude that fewer than half of adult Americans can name at least one currently serving Supreme Court justice.\n")
}




# Given data
sample_mean <- 121.3
population_mean_null <- 129
population_sd <- 47
sample_size <- 247

# Calculate the standard error
standard_error <- population_sd / sqrt(sample_size)

# Calculate the t-statistic
t_statistic <- (sample_mean - population_mean_null) / standard_error

# Calculate the degrees of freedom for a one-sample t-test
degrees_of_freedom <- sample_size - 1

# Calculate the p-value
p_value <- pt(t_statistic, df = degrees_of_freedom)

# Print the results
cat("Sample Mean:", sample_mean, "\n")
cat("T-Statistic:", t_statistic, "\n")
cat("P-Value:", p_value, "\n")

# Compare p-value to significance level (e.g., 0.05)
alpha <- 0.05
if (p_value < alpha) {
  cat("Reject the null hypothesis. There is evidence that heavy internet users watch less TV than the typical American.\n")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to conclude that heavy internet users watch less TV than the typical American.\n")
}






# Install and load ggplot2 if not already installed
# install.packages("ggplot2")
library(ggplot2)

# Define the values for the x-axis (e.g., from -4 to 4)
x_values <- seq(-4, 4, by = 0.01)

# Create a data frame with x values and corresponding probabilities
df <- data.frame(x = x_values, p = pnorm(x_values))

# Create the plot
ggplot(df, aes(x = x, y = p)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymax = p, ymin = 0), fill = "gray", alpha = 0.3) +
  geom_text(aes(label = sprintf("%.2f%%", p * 100), x = -2.58, y = p), vjust = -0.5, color = "red") +
  labs(title = "Standard Normal Distribution",
       x = "Z-Score",
       y = "Cumulative Probability") +
  theme_minimal()



# Install and load the ggplot2 package if not already installed
# install.packages("ggplot2")
library(ggplot2)

# Value for which to calculate the CDF
value <- -2.58

# Create a sequence of values for the x-axis
x_values <- seq(-3, 3, length.out = 1000)

# Calculate the CDF values for each x value
cdf_values <- pnorm(x_values)

# Create a data frame
df <- data.frame(x = x_values, cdf = cdf_values)

# Plot the CDF curve
ggplot(df, aes(x = x, y = cdf)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(data = subset(df, x <= value), aes(ymax = cdf), fill = "gray", alpha = 0.5) +
  geom_vline(xintercept = value, linetype = "dashed", color = "red") +
  labs(title = "Standard Normal Distribution CDF",
       x = "Z-Score",
       y = "Cumulative Probability") +
  theme_minimal()


