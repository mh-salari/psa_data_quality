# Load required libraries
library(tidyverse)

# Load the data
path <-  file.path("..", "quality_metrics", "apparent_gaze_shift.csv")
apparent_gaze_shift_df <- read_csv(path, show_col_types = FALSE)

# First, aggregate the data by averaging across repeats for each participant/eye tracker
aggregated_data <- apparent_gaze_shift_df %>%
  group_by(participant_id, eye_tracker) %>%
  summarize(apparent_gaze_shift = mean(apparent_gaze_shift, na.rm = TRUE), .groups = "drop")

# ============= BASIC DESCRIPTIVE STATISTICS ===============

# Calculate basic descriptive statistics by eye tracker
basic_stats <- apparent_gaze_shift_df %>%
  group_by(eye_tracker) %>%
  summarize(
    n = n(),
    mean = mean(apparent_gaze_shift, na.rm = TRUE),
    median = median(apparent_gaze_shift, na.rm = TRUE),
    sd = sd(apparent_gaze_shift, na.rm = TRUE),
    min = min(apparent_gaze_shift, na.rm = TRUE),
    max = max(apparent_gaze_shift, na.rm = TRUE),
    q25 = quantile(apparent_gaze_shift, 0.25, na.rm = TRUE),
    q75 = quantile(apparent_gaze_shift, 0.75, na.rm = TRUE),
    abs_mean = mean(abs(apparent_gaze_shift), na.rm = TRUE),  # Mean of absolute apparent_gaze_shift values
    .groups = "drop"
  )

# Print basic statistics in a nice format
cat("\n================================================================\n")
cat("Basic Descriptive Statistics of apparent_gaze_shift by Eye Tracker\n")
cat("================================================================\n\n")

for (i in 1:nrow(basic_stats)) {
  cat("Eye Tracker:", basic_stats$eye_tracker[i], "\n")
  cat("Number of observations:", basic_stats$n[i], "\n")
  cat("Mean apparent_gaze_shift:", round(basic_stats$mean[i], 3), "degrees (SD =", round(basic_stats$sd[i], 3), ")\n")
  cat("Median apparent_gaze_shift:", round(basic_stats$median[i], 3), "degrees\n")
  cat("Range:", round(basic_stats$min[i], 3), "to", round(basic_stats$max[i], 3), "degrees\n")
  cat("Interquartile Range:", round(basic_stats$q25[i], 3), "to", round(basic_stats$q75[i], 3), "degrees\n")
  cat("Mean Absolute apparent_gaze_shift:", round(basic_stats$abs_mean[i], 3), "degrees\n\n")
}

# ============= STATISTICAL ANALYSIS ===============

# Perform statistical tests
stat_results <- data.frame()

# For each eye tracker, test if apparent_gaze_shift is significantly different from zero
for (tracker in unique(aggregated_data$eye_tracker)) {
  # Subset data for this eye tracker
  tracker_data <- aggregated_data[aggregated_data$eye_tracker == tracker, ]
  
  # One-sample t-test against zero
  t_test_result <- t.test(tracker_data$apparent_gaze_shift, mu = 0)
  
  # Calculate Cohen's d for one-sample t-test
  cohens_d <- mean(tracker_data$apparent_gaze_shift) / sd(tracker_data$apparent_gaze_shift)
  
  # Interpret effect size
  effect_interpretation <- case_when(
    abs(cohens_d) < 0.2 ~ "Negligible",
    abs(cohens_d) < 0.5 ~ "Small",
    abs(cohens_d) < 0.8 ~ "Medium",
    TRUE ~ "Large"
  )
  
  # Store results
  new_row <- data.frame(
    eye_tracker = tracker,
    n_samples = nrow(tracker_data),
    mean_apparent_gaze_shift = mean(tracker_data$apparent_gaze_shift),
    sd_apparent_gaze_shift = sd(tracker_data$apparent_gaze_shift),
    t_statistic = t_test_result$statistic,
    df = t_test_result$parameter,
    p_value = t_test_result$p.value,
    cohens_d = cohens_d,
    effect_size = effect_interpretation
  )
  
  stat_results <- rbind(stat_results, new_row)
}

# Print statistical results in a nice format
cat("\n===============================================================\n")
cat("Statistical Analysis of apparent_gaze_shift (One-sample t-test against zero)\n")
cat("===============================================================\n")

for (i in 1:nrow(stat_results)) {
  cat("\nEye Tracker:", stat_results$eye_tracker[i], "\n")
  cat("Sample size:", stat_results$n_samples[i], "\n")
  cat("Mean apparent_gaze_shift:", round(stat_results$mean_apparent_gaze_shift[i], 3), "degrees",
      "(SD =", round(stat_results$sd_apparent_gaze_shift[i], 3), ")\n")
  cat("t-statistic:", round(stat_results$t_statistic[i], 3), 
      "(df =", round(stat_results$df[i], 1), ")\n")
  cat("p-value:", format.pval(stat_results$p_value[i], digits = 3), 
      ifelse(stat_results$p_value[i] < 0.05, " (significant)", " (not significant)"), "\n")
  cat("Cohen's d:", round(stat_results$cohens_d[i], 3), 
      paste0("(", stat_results$effect_size[i], " effect)"), "\n")
}

# Create a formatted table for potential use in publication
formatted_table <- stat_results %>%
  mutate(
    tracker = eye_tracker,
    mean_sd = sprintf("%.3f (%.3f)", mean_apparent_gaze_shift, sd_apparent_gaze_shift),
    test_results = sprintf("t(%0.1f) = %0.2f, %s", 
                           df, 
                           t_statistic, 
                           ifelse(p_value < 0.001, "p < 0.001", 
                                  ifelse(p_value < 0.01, "p < 0.01", 
                                         ifelse(p_value < 0.05, "p < 0.05", 
                                                sprintf("p = %0.3f", p_value))))),
    effect = sprintf("%0.2f (%s)", cohens_d, effect_size)
  ) %>%
  select(tracker, n_samples, mean_sd, test_results, effect)

# Print the formatted table
cat("\n\nFormatted Table for Publication:\n")
print(formatted_table)

# Save the statistical results to a CSV file
write.csv(stat_results, "./output/apparent_gaze_shift_statistical_analysis.csv", row.names = FALSE)

# Save basic statistics to CSV in the current working directory
write.csv(basic_stats, file = "./output/apparent_gaze_shift_descriptive_stats.csv", row.names = FALSE)

# Print a message confirming where files were saved
cat("\nFiles saved to current working directory:", getwd(), "\n")
cat("Saved files:\n")
cat("- ./output/apparent_gaze_shift_statistical_analysis.csv\n")
cat("- ./output/apparent_gaze_shift_descriptive_stats.csv\n")