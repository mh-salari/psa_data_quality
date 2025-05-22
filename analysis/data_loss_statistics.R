# Load required libraries
library(dplyr)
library(tidyr)

# Read the CSV files
path1 <- file.path("..", "data", "hm_nan_statistics.csv")
df1 <- read.csv(path1)
path2 <- file.path("..", "data", "eyelink1000plus_nan_statistics.csv")
df2 <- read.csv(path2)

# Ensure column names are consistent
names(df1)[names(df1) == "condition"] <- "trial_condition"
names(df2)[names(df2) == "condition"] <- "trial_condition"

# Combine the two dataframes
df <- rbind(df1, df2)

# Calculate data loss percentage for each row 
df$data_loss <- (df$nan_rows / df$total_rows) * 100

# First, aggregate the data by averaging across repeats for each participant/condition/eye tracker
# (assuming participant_id exists in your data, adjust if column name is different)
aggregated_data <- df %>%
  group_by(participant_id, eye_tracker, trial_condition) %>%
  summarize(data_loss = mean(data_loss, na.rm = TRUE), .groups = "drop")

# ============= BASIC DESCRIPTIVE STATISTICS ===============

# Calculate basic descriptive statistics by eye tracker and condition
basic_stats <- df %>%
  group_by(eye_tracker, trial_condition) %>%
  summarize(
    n = n(),
    mean = mean(data_loss, na.rm = TRUE),
    median = median(data_loss, na.rm = TRUE),
    sd = sd(data_loss, na.rm = TRUE),
    min = min(data_loss, na.rm = TRUE),
    max = max(data_loss, na.rm = TRUE),
    q25 = quantile(data_loss, 0.25, na.rm = TRUE),
    q75 = quantile(data_loss, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# Print basic statistics in a well-formatted manner
cat("\n================================================================\n")
cat("Basic Descriptive Statistics of Data Loss by Eye Tracker and Condition\n")
cat("================================================================\n\n")

# Get unique eye trackers for organized display
trackers <- unique(basic_stats$eye_tracker)

for (tracker in trackers) {
  cat("Eye Tracker:", tracker, "\n\n")
  
  # Get data for this tracker
  tracker_data <- basic_stats[basic_stats$eye_tracker == tracker, ]
  
  for (i in 1:nrow(tracker_data)) {
    cat("  Condition:", tracker_data$trial_condition[i], "\n")
    cat("  Number of observations:", tracker_data$n[i], "\n")
    cat("  Mean data loss:", round(tracker_data$mean[i], 3), "% (SD =", round(tracker_data$sd[i], 3), "%)\n")
    cat("  Median data loss:", round(tracker_data$median[i], 3), "%\n")
    cat("  Range:", round(tracker_data$min[i], 3), "to", round(tracker_data$max[i], 3), "%\n")
    cat("  Interquartile Range:", round(tracker_data$q25[i], 3), "to", round(tracker_data$q75[i], 3), "%\n\n")
  }
  
  cat("-----------------------------------------\n\n")
}

# ============= STATISTICAL ANALYSIS ===============

# Perform statistical tests
stat_results <- data.frame()

# Count the number of trackers for Bonferroni correction
num_trackers <- length(unique(aggregated_data$eye_tracker))

# For each eye tracker, compare dark vs bright conditions
for (tracker in unique(aggregated_data$eye_tracker)) {
  # Subset data for this eye tracker
  tracker_data <- aggregated_data[aggregated_data$eye_tracker == tracker, ]
  
  # Create a dataframe that ensures paired data (same participants in both conditions)
  paired_data <- tracker_data %>%
    pivot_wider(
      id_cols = participant_id,
      names_from = trial_condition,
      values_from = data_loss
    ) %>%
    filter(!is.na(dark) & !is.na(bright))
  
  if (nrow(paired_data) >= 3) {  # Enough paired samples for a paired test
    # Paired t-test
    t_test_result <- t.test(paired_data$dark, paired_data$bright, paired = TRUE)
    test_type <- "Paired t-test"
    
    # Calculate Cohen's d for paired data
    diff <- paired_data$dark - paired_data$bright
    cohens_d <- mean(diff) / sd(diff)
    n_samples <- nrow(paired_data)
    
    # Calculate descriptive statistics
    dark_mean <- mean(paired_data$dark)
    dark_sd <- sd(paired_data$dark)
    bright_mean <- mean(paired_data$bright)
    bright_sd <- sd(paired_data$bright)
  } else {
    # If not enough paired data, fall back to independent samples test
    dark_data <- tracker_data$data_loss[tracker_data$trial_condition == "dark"]
    bright_data <- tracker_data$data_loss[tracker_data$trial_condition == "bright"]
    
    # Independent t-test
    t_test_result <- t.test(dark_data, bright_data, paired = FALSE, var.equal = FALSE)
    test_type <- "Independent t-test"
    
    # Cohen's d for independent samples
    pooled_sd <- sqrt(((length(dark_data)-1)*var(dark_data) + 
                         (length(bright_data)-1)*var(bright_data)) / 
                        (length(dark_data) + length(bright_data) - 2))
    
    cohens_d <- (mean(dark_data) - mean(bright_data)) / pooled_sd
    n_samples <- paste(length(dark_data), "&", length(bright_data))
    
    # Calculate descriptive statistics
    dark_mean <- mean(dark_data)
    dark_sd <- sd(dark_data)
    bright_mean <- mean(bright_data)
    bright_sd <- sd(bright_data)
  }
  
  # Get the unadjusted p-value
  p_value <- t_test_result$p.value
  
  # Apply Bonferroni correction
  p_value_adjusted <- min(p_value * num_trackers, 1.0)
  
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
    test_type = test_type,
    n_samples = ifelse(is.character(n_samples), n_samples, as.character(n_samples)),
    dark_mean = dark_mean,
    dark_sd = dark_sd,
    bright_mean = bright_mean,
    bright_sd = bright_sd,
    t_statistic = t_test_result$statistic,
    df = t_test_result$parameter,
    p_value = p_value,
    p_value_adjusted = p_value_adjusted,
    mean_diff = ifelse(test_type == "Paired t-test", 
                       t_test_result$estimate, 
                       t_test_result$estimate[1] - t_test_result$estimate[2]),
    cohens_d = cohens_d,
    effect_size = effect_interpretation
  )
  
  stat_results <- rbind(stat_results, new_row)
}

# Print statistical results in a structured format
cat("\n===============================================================\n")
cat("Statistical Analysis of Data Loss Differences (dark vs. bright)\n")
cat("===============================================================\n")
cat("Bonferroni Correction Applied (", num_trackers, " tests, alpha adjusted from 0.05 to ", round(0.05/num_trackers, 4), ")\n", sep="")

for (i in 1:nrow(stat_results)) {
  cat("\nEye Tracker:", stat_results$eye_tracker[i], "\n")
  cat("Test type:", stat_results$test_type[i], "\n")
  cat("Sample size:", stat_results$n_samples[i], "\n")
  cat("dark condition: M =", round(stat_results$dark_mean[i], 3), 
      "% (SD =", round(stat_results$dark_sd[i], 3), "%)\n")
  cat("bright condition: M =", round(stat_results$bright_mean[i], 3), 
      "% (SD =", round(stat_results$bright_sd[i], 3), "%)\n")
  cat("t-statistic:", round(stat_results$t_statistic[i], 3), 
      "(df =", round(stat_results$df[i], 1), ")\n")
  cat("Unadjusted p-value:", format.pval(stat_results$p_value[i], digits = 3), 
      ifelse(stat_results$p_value[i] < 0.05, " (significant)", " (not significant)"), "\n")
  cat("Bonferroni-adjusted p-value:", format.pval(stat_results$p_value_adjusted[i], digits = 3), 
      ifelse(stat_results$p_value_adjusted[i] < 0.05, " (significant)", " (not significant)"), "\n")
  cat("Mean difference:", round(stat_results$mean_diff[i], 3), "%\n")
  cat("Cohen's d:", round(stat_results$cohens_d[i], 3), 
      paste0("(", stat_results$effect_size[i], " effect)"), "\n")
}

# Calculate overall statistics across all eye trackers
overall_stats <- aggregated_data %>%
  group_by(trial_condition) %>%
  summarize(
    mean = mean(data_loss, na.rm = TRUE),
    sd = sd(data_loss, na.rm = TRUE),
    n = n()
  )

# Perform overall t-test (independent samples, as we're combining across different eye trackers)
dark_overall <- aggregated_data$data_loss[aggregated_data$trial_condition == "dark"]
bright_overall <- aggregated_data$data_loss[aggregated_data$trial_condition == "bright"]
overall_t_test <- t.test(dark_overall, bright_overall, var.equal = FALSE)

# Calculate overall Cohen's d
pooled_sd_overall <- sqrt(((length(dark_overall)-1)*var(dark_overall) + 
                             (length(bright_overall)-1)*var(bright_overall)) / 
                            (length(dark_overall) + length(bright_overall) - 2))
cohens_d_overall <- (mean(dark_overall) - mean(bright_overall)) / pooled_sd_overall

# For the overall test, Bonferroni correction is not applied as it's just one test
p_value_overall <- overall_t_test$p.value

# Interpret effect size
effect_overall <- case_when(
  abs(cohens_d_overall) < 0.2 ~ "Negligible",
  abs(cohens_d_overall) < 0.5 ~ "Small",
  abs(cohens_d_overall) < 0.8 ~ "Medium",
  TRUE ~ "Large"
)

# Print overall results
cat("\n\n===============================================================\n")
cat("Overall Statistical Analysis (All Eye Trackers Combined)\n")
cat("===============================================================\n")
cat("dark condition: M =", round(overall_stats$mean[overall_stats$trial_condition == "dark"], 3), 
    "% (SD =", round(overall_stats$sd[overall_stats$trial_condition == "dark"], 3), 
    "%, n =", overall_stats$n[overall_stats$trial_condition == "dark"], ")\n")
cat("bright condition: M =", round(overall_stats$mean[overall_stats$trial_condition == "bright"], 3), 
    "% (SD =", round(overall_stats$sd[overall_stats$trial_condition == "bright"], 3), 
    "%, n =", overall_stats$n[overall_stats$trial_condition == "bright"], ")\n")
cat("t-statistic:", round(overall_t_test$statistic, 3), 
    "(df =", round(overall_t_test$parameter, 1), ")\n")
cat("p-value:", format.pval(p_value_overall, digits = 3), 
    ifelse(p_value_overall < 0.05, " (significant)", " (not significant)"), "\n")
cat("Mean difference:", round(overall_t_test$estimate[1] - overall_t_test$estimate[2], 3), "%\n")
cat("Cohen's d:", round(cohens_d_overall, 3), 
    paste0("(", effect_overall, " effect)"), "\n")
cat("Note: Bonferroni correction was not applied to the overall test as it is a single comparison.\n")

# Create a formatted table for potential use in publication
formatted_table <- stat_results %>%
  mutate(
    tracker = eye_tracker,
    condition = paste0("dark: ", sprintf("%.3f (%.3f)", dark_mean, dark_sd), 
                       "% bright: ", sprintf("%.3f (%.3f)", bright_mean, bright_sd), "%"),
    test_results = sprintf("t(%0.1f) = %0.2f", df, t_statistic),
    p_values = sprintf("p = %s, p_adj = %s", 
                       ifelse(p_value < 0.001, "<0.001", 
                              ifelse(p_value < 0.01, "<0.01", 
                                     ifelse(p_value < 0.05, "<0.05", 
                                            sprintf("%.3f", p_value)))),
                       ifelse(p_value_adjusted < 0.001, "<0.001", 
                              ifelse(p_value_adjusted < 0.01, "<0.01", 
                                     ifelse(p_value_adjusted < 0.05, "<0.05", 
                                            sprintf("%.3f", p_value_adjusted))))),
    significance = ifelse(p_value < 0.05, 
                          ifelse(p_value_adjusted < 0.05, "Both sig.", "Only p sig."), 
                          "Not sig."),
    effect = sprintf("%0.2f (%s)", cohens_d, effect_size)
  ) %>%
  select(tracker, test_type, n_samples, condition, test_results, p_values, significance, effect)

# Print the formatted table
cat("\n\nFormatted Table for Publication (with both p-values):\n")
print(formatted_table)

# Save the statistical results to a CSV file
write.csv(stat_results, "./output/data_loss_statistical_analysis.csv", row.names = FALSE)

# Save basic statistics to CSV in the current working directory
write.csv(basic_stats, file = "./output/data_loss_descriptive_stats.csv", row.names = FALSE)

# Print a message confirming where files were saved
cat("\nFiles saved to current working directory:", getwd(), "\n")
cat("Saved files:\n")
cat("- ./output/data_loss_statistical_analysis.csv\n")
cat("- ./output/data_loss_descriptive_stats.csv\n")