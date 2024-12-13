setwd("C:/Users/luish/OneDrive - University of Leeds/4. MATH5741M Statistical Theory and Methods (33874)/coursework 1")
#load('rehoming.RData')
#createsample(201899113)
#save(mysample, file = 'mysample.RData')

load('mysample.RData')

# 1. Data Cleaning-----
mysample_cleaned = mysample[mysample$Rehomed != 99999 & !is.na(mysample$Breed),]

# 2. Data Exploration-----
# splitting sample by breed
breed_groups = split(mysample_cleaned, mysample_cleaned$Breed) # three list

# Calculate mean and variance for each breed
# Function to calculate mean and variance for each breed
calc_mean_variance <- function(data, breed_name) {
  mean_value <- mean(data$Rehomed)
  variance_value <- var(data$Rehomed)
  cat(breed_name, ": Mean =", mean_value, ", Variance =", variance_value, "\n")
  return(list(mean = mean_value, variance = variance_value))
}
# Apply the function to each breed group
mixed_stats <- calc_mean_variance(breed_groups$`Mixed Breed`, "Mixed Breed")
labrador_stats <- calc_mean_variance(breed_groups$`Labrador Retriever`, "Labrador Retriever")
bichon_stats <- calc_mean_variance(breed_groups$`Bichon Frise`, "Bichon Frise")

# Histograms----
breeds <- unique(mysample_cleaned$Breed)
breaks <- seq(min(mysample_cleaned$Rehomed), max(mysample_cleaned$Rehomed) + 1, 1)
par(mfrow = c(length(breeds), 1))  # Set up the plotting area
for (breed in breeds) {
  hist(mysample_cleaned$Rehomed[mysample_cleaned$Breed == breed], 
       #breaks = 10,
       main = paste("Distribution of Rehoming Time for", breed), 
       xlab = "Rehoming Time (weeks)", 
       col = "lightblue", 
       border = "black",
       freq = FALSE,)
}

# 3. Modelling and estimation Normal Distribution-----
# Set up a 1x3 layout
par(mfrow = c(1, 3))

# Mixed Breed
qqnorm(breed_groups$`Mixed Breed`$Rehomed, main = "Q-Q Plot: Mixed Breed")
qqline(breed_groups$`Mixed Breed`$Rehomed, col = "blue")

# Labrador Retriever
qqnorm(breed_groups$`Labrador Retriever`$Rehomed, main = "Q-Q Plot: Labrador Retriever")
qqline(breed_groups$`Labrador Retriever`$Rehomed, col = "blue")

# Bichon Frise
qqnorm(breed_groups$`Bichon Frise`$Rehomed, main = "Q-Q Plot: Bichon Frise")
qqline(breed_groups$`Bichon Frise`$Rehomed, col = "blue")

# Evaluating with Kolmogorov-Smirnov (KS), Shapiro-Wilk, and Chi-squared tests----
# Evaluation fit to normality kolmogorov-Smirnov test
ks_mixed <- ks.test(breed_groups$`Mixed Breed`$Rehomed, "pnorm", 
                    mean = mean(breed_groups$`Mixed Breed`$Rehomed), 
                    sd = sd(breed_groups$`Mixed Breed`$Rehomed))

ks_labrador <- ks.test(breed_groups$`Labrador Retriever`$Rehomed, "pnorm", 
                       mean = mean(breed_groups$`Labrador Retriever`$Rehomed), 
                       sd = sd(breed_groups$`Labrador Retriever`$Rehomed))

ks_bichon <- ks.test(breed_groups$`Bichon Frise`$Rehomed, "pnorm", 
                     mean = mean(breed_groups$`Bichon Frise`$Rehomed), 
                     sd = sd(breed_groups$`Bichon Frise`$Rehomed))


# Evaluation fit to normality shapiro-wilk test
shapiro_mixed <- shapiro.test(breed_groups$`Mixed Breed`$Rehomed)
shapiro_labrador <- shapiro.test(breed_groups$`Labrador Retriever`$Rehomed)
shapiro_bichon <- shapiro.test(breed_groups$`Bichon Frise`$Rehomed)

# Pearson Chi-Squared Normality test
install.packages("nortest")
library(nortest)
pearson_mixed <- pearson.test(breed_groups$`Mixed Breed`$Rehomed)
pearson_labrador <- pearson.test(breed_groups$`Labrador Retriever`$Rehomed)
pearson_bichon <- pearson.test(breed_groups$`Bichon Frise`$Rehomed)

# summary in table
# Combine results
results <- data.frame(
  Breed = c("Mixed Breed", "Labrador Retriever", "Bichon Frise"),
  KS_p_value = c(ks_mixed$p.value, ks_labrador$p.value, ks_bichon$p.value),
  Shapiro_p_value = c(shapiro_mixed$p.value, shapiro_labrador$p.value, shapiro_bichon$p.value),
  Pearson_p_value = c(pearson_mixed$p.value, pearson_labrador$p.value, pearson_bichon$p.value)
)

# Print results
print(results)

# 4 Inference: CI----

# Function to calculate confidence intervals
calc_confidence_interval <- function(data, mu, method) {
  n <- length(data)
  mean_value <- mean(data)
  sd_value <- sd(data)
  se <- sd_value / sqrt(n)  # Standard error
  
  if (method == "z-test") {
    z_value <- qnorm(0.975)  # 95% confidence level
    ci <- c(mean_value - z_value * se, mean_value + z_value * se)
  } else if (method == "t-test") {
    t_value <- qt(0.975, df = n - 1)
    ci <- c(mean_value - t_value * se, mean_value + t_value * se)
  }
  return(list(mean = mean_value, ci = ci))
}

# Mixed Breed: Use z-test
mixed_results <- calc_confidence_interval(breed_groups$`Mixed Breed`$Rehomed, mu = 27, method = "z-test")

# Labrador Retriever: Use z-test
labrador_results <- calc_confidence_interval(breed_groups$`Labrador Retriever`$Rehomed, mu = 27, method = "z-test")

# Bichon Frise: Use t-test
bichon_results <- calc_confidence_interval(breed_groups$`Bichon Frise`$Rehomed, mu = 27, method = "t-test")

# Combine Results into a Data Frame
results <- data.frame(
  Breed = c("Mixed Breed", "Labrador Retriever", "Bichon Frise"),
  Method = c("z-test", "z-test", "t-test"),
  Mean = c(mixed_results$mean, labrador_results$mean, bichon_results$mean),
  CI_Lower = c(mixed_results$ci[1], labrador_results$ci[1], bichon_results$ci[1]),
  CI_Upper = c(mixed_results$ci[2], labrador_results$ci[2], bichon_results$ci[2])
)

# Print Results
print(results)
# Visualization of Confidence Intervals
library(ggplot2)

# Create a data frame for plotting
plot_data <- data.frame(
  Breed = c("Mixed Breed", "Labrador Retriever", "Bichon Frise"),
  Mean = c(mixed_results$mean, labrador_results$mean, bichon_results$mean),
  CI_Lower = c(mixed_results$ci[1], labrador_results$ci[1], bichon_results$ci[1]),
  CI_Upper = c(mixed_results$ci[2], labrador_results$ci[2], bichon_results$ci[2])
)

# Plot with annotations for confidence intervals

# Adjust x-axis limits to accommodate annotations
x_max <- max(plot_data$CI_Upper) + 5  # Add a buffer for annotations

ggplot(plot_data, aes(y = Breed, x = Mean)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2) +
  geom_vline(xintercept = 27, linetype = "dashed", color = "red") +
  geom_text(aes(label = paste0("(", round(CI_Lower, 2), ", ", round(CI_Upper, 2), ")"), 
                x = CI_Upper + 1), # Position text slightly to the right of the CI upper bound
            size = 3.5, hjust = 0) +
  labs(title = "Confidence Intervals for Mean Rehoming Time",
       x = "Mean Rehoming Time (weeks)", y = "Breed") +
  theme_minimal()+
  xlim(NA,x_max)

# 5. paired samples: independant samples---- 

# Function to calculate confidence interval for the difference in means
calc_mean_diff_ci <- function(data1, data2, equal_var = TRUE) {
  n1 <- length(data1)
  n2 <- length(data2)
  mean1 <- mean(data1)
  mean2 <- mean(data2)
  sd1 <- sd(data1)
  sd2 <- sd(data2)
  if (equal_var) {
    # Pooled standard deviation
    pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
    se <- pooled_sd * sqrt(1/n1 + 1/n2)
    df <- n1 + n2 - 2
  } else {
    # Welch's t-test standard error
    se <- sqrt(sd1^2/n1 + sd2^2/n2)
    df <- ((sd1^2/n1 + sd2^2/n2)^2) / 
      ((sd1^2/n1)^2 / (n1 - 1) + (sd2^2/n2)^2 / (n2 - 1))
  }
  t_value <- qt(0.975, df = df)
  ci <- c((mean1 - mean2) - t_value * se, (mean1 - mean2) + t_value * se)
  return(list(mean_diff = mean1 - mean2, ci = ci))
}

# Pairwise comparisons
mixed_vs_lab <- calc_mean_diff_ci(breed_groups$`Mixed Breed`$Rehomed, 
                                  breed_groups$`Labrador Retriever`$Rehomed, equal_var = TRUE)
mixed_vs_bichon <- calc_mean_diff_ci(breed_groups$`Mixed Breed`$Rehomed, 
                                     breed_groups$`Bichon Frise`$Rehomed, equal_var = TRUE)
lab_vs_bichon <- calc_mean_diff_ci(breed_groups$`Labrador Retriever`$Rehomed, 
                                   breed_groups$`Bichon Frise`$Rehomed, equal_var = TRUE)

# Combine results into a data frame
pairwise_results <- data.frame(
  Comparison = c("Mixed Breed vs Labrador Retriever", 
                 "Mixed Breed vs Bichon Frise", 
                 "Labrador Retriever vs Bichon Frise"),
  Mean_Diff = c(mixed_vs_lab$mean_diff, mixed_vs_bichon$mean_diff, lab_vs_bichon$mean_diff),
  CI_Lower = c(mixed_vs_lab$ci[1], mixed_vs_bichon$ci[1], lab_vs_bichon$ci[1]),
  CI_Upper = c(mixed_vs_lab$ci[2], mixed_vs_bichon$ci[2], lab_vs_bichon$ci[2])
)


# Caterpillar plot with confidence intervals shown to the right
library(ggplot2)
# Adjust x-axis limits to accommodate annotations
x_max <- max(pairwise_results$CI_Upper) + 5
ggplot(pairwise_results, aes(x = Comparison, y = Mean_Diff)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_text(aes(label = paste0("(", round(CI_Lower, 2), ", ", round(CI_Upper, 2), ")"), 
                y = CI_Upper + 0.2), # Position text slightly above the upper CI bound
            hjust = -0.1, size = 3.5) +
  labs(title = "Pairwise Confidence Intervals for Mean Differences",
       x = "Breed Comparison", y = "Mean Difference (weeks)") +
  coord_flip() +  # Flip for caterpillar style
  scale_y_continuous(limits = c(NA, x_max)) +  # Use xlim to set upper bound
  theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))  # Further adjust margins for more space on the right

