# Load required libraries
library(tidyverse)

# Load the std data
path <-  file.path("..", "quality_metrics", "std.csv")
std_df <- read_csv(path, show_col_types = FALSE)

# First, aggregate the data by averaging across repeats for each participant/condition/eye tracker
aggregated_data <- std_df %>%
  group_by(participant_id, eye_tracker, trial_condition) %>%
  summarize(std = mean(std, na.rm = TRUE), .groups = "drop")

# ============= BASIC DESCRIPTIVE STATISTICS ===============

# Calculate basic descriptive statistics by eye tracker and condition
basic_stats <- aggregated_data %>%
  group_by(eye_tracker, trial_condition) %>%
  summarize(
    n = n(),
    mean = mean(std, na.rm = TRUE),
    median = median(std, na.rm = TRUE),
    sd = sd(std, na.rm = TRUE),
    min = min(std, na.rm = TRUE),
    max = max(std, na.rm = TRUE),
    q25 = quantile(std, 0.25, na.rm = TRUE),
    q75 = quantile(std, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# Print basic statistics in a clear format
cat("\n================================================================\n")
cat("Basic Descriptive Statistics of std by Eye Tracker and Condition\n")
cat("================================================================\n\n")

# Get unique eye trackers
trackers <- unique(basic_stats$eye_tracker)

for (tracker in trackers) {
  cat("Eye Tracker:", tracker, "\n\n")
  
  # Get data for this tracker
  tracker_data <- basic_stats[basic_stats$eye_tracker == tracker, ]
  
  for (i in 1:nrow(tracker_data)) {
    cat("  Condition:", tracker_data$trial_condition[i], "\n")
    cat("  Number of observations:", tracker_data$n[i], "\n")
    cat("  Mean std:", round(tracker_data$mean[i], 3), "(SD =", round(tracker_data$sd[i], 3), ")\n")
    cat("  Median std:", round(tracker_data$median[i], 3), "\n")
    cat("  Range:", round(tracker_data$min[i], 3), "to", round(tracker_data$max[i], 3), "\n")
    cat("  Interquartile Range:", round(tracker_data$q25[i], 3), "to", round(tracker_data$q75[i], 3), "\n\n")
  }
  
  cat("-----------------------------------------\n\n")
}

# ============= STATISTICAL ANALYSIS ===============

# Perform statistical tests
stat_results <- data.frame()

# For each eye tracker, compare dilated vs constricted conditions
for (tracker in unique(aggregated_data$eye_tracker)) {
  # Subset data for this eye tracker
  tracker_data <- aggregated_data[aggregated_data$eye_tracker == tracker, ]
  
  # Create a dataframe that ensures paired data (same participants in both conditions)
  paired_data <- tracker_data %>%
    pivot_wider(
      id_cols = participant_id,
      names_from = trial_condition,
      values_from = std
    ) %>%
    filter(!is.na(dilated) & !is.na(constricted))
  
  if (nrow(paired_data) >= 3) {  # Enough paired samples for a paired test
    # Paired t-test
    t_test_result <- t.test(paired_data$dilated, paired_data$constricted, paired = TRUE)
    test_type <- "Paired t-test"
    
    # Calculate Cohen's d for paired data
    diff <- paired_data$dilated - paired_data$constricted
    cohens_d <- mean(diff) / sd(diff)
    n_samples <- nrow(paired_data)
    
    # Calculate descriptive statistics
    dilated_mean <- mean(paired_data$dilated)
    dilated_sd <- sd(paired_data$dilated)
    constricted_mean <- mean(paired_data$constricted)
    constricted_sd <- sd(paired_data$constricted)
  } else {
    # If not enough paired data, fall back to independent samples test
    dilated_data <- tracker_data$std[tracker_data$trial_condition == "dilated"]
    constricted_data <- tracker_data$std[tracker_data$trial_condition == "constricted"]
    
    # Independent t-test
    t_test_result <- t.test(dilated_data, constricted_data, paired = FALSE, var.equal = FALSE)
    test_type <- "Independent t-test"
    
    # Cohen's d for independent samples
    pooled_sd <- sqrt(((length(dilated_data)-1)*var(dilated_data) + 
                         (length(constricted_data)-1)*var(constricted_data)) / 
                        (length(dilated_data) + length(constricted_data) - 2))
    
    cohens_d <- (mean(dilated_data) - mean(constricted_data)) / pooled_sd
    n_samples <- paste(length(dilated_data), "&", length(constricted_data))
    
    # Calculate descriptive statistics
    dilated_mean <- mean(dilated_data)
    dilated_sd <- sd(dilated_data)
    constricted_mean <- mean(constricted_data)
    constricted_sd <- sd(constricted_data)
  }
  
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
    dilated_mean = dilated_mean,
    dilated_sd = dilated_sd,
    constricted_mean = constricted_mean,
    constricted_sd = constricted_sd,
    t_statistic = t_test_result$statistic,
    df = t_test_result$parameter,
    p_value = t_test_result$p.value,
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
cat("Statistical Analysis of std Differences (dilated vs. constricted)\n")
cat("===============================================================\n")

for (i in 1:nrow(stat_results)) {
  cat("\nEye Tracker:", stat_results$eye_tracker[i], "\n")
  cat("Test type:", stat_results$test_type[i], "\n")
  cat("Sample size:", stat_results$n_samples[i], "\n")
  cat("dilated condition: M =", round(stat_results$dilated_mean[i], 3), 
      "(SD =", round(stat_results$dilated_sd[i], 3), ")\n")
  cat("constricted condition: M =", round(stat_results$constricted_mean[i], 3), 
      "(SD =", round(stat_results$constricted_sd[i], 3), ")\n")
  cat("t-statistic:", round(stat_results$t_statistic[i], 3), 
      "(df =", round(stat_results$df[i], 1), ")\n")
  cat("p-value:", format.pval(stat_results$p_value[i], digits = 3), 
      ifelse(stat_results$p_value[i] < 0.05, " (significant)", " (not significant)"), "\n")
  cat("Mean difference:", round(stat_results$mean_diff[i], 3), "\n")
  cat("Cohen's d:", round(stat_results$cohens_d[i], 3), 
      paste0("(", stat_results$effect_size[i], " effect)"), "\n")
}

# Calculate overall statistics across all eye trackers
overall_stats <- aggregated_data %>%
  group_by(trial_condition) %>%
  summarize(
    mean = mean(std, na.rm = TRUE),
    sd = sd(std, na.rm = TRUE),
    n = n()
  )

# Perform overall t-test (independent samples, as we're combining across different eye trackers)
dilated_overall <- aggregated_data$std[aggregated_data$trial_condition == "dilated"]
constricted_overall <- aggregated_data$std[aggregated_data$trial_condition == "constricted"]
overall_t_test <- t.test(dilated_overall, constricted_overall, var.equal = FALSE)

# Calculate overall Cohen's d
pooled_sd_overall <- sqrt(((length(dilated_overall)-1)*var(dilated_overall) + 
                             (length(constricted_overall)-1)*var(constricted_overall)) / 
                            (length(dilated_overall) + length(constricted_overall) - 2))
cohens_d_overall <- (mean(dilated_overall) - mean(constricted_overall)) / pooled_sd_overall

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
cat("dilated condition: M =", round(overall_stats$mean[overall_stats$trial_condition == "dilated"], 3), 
    "(SD =", round(overall_stats$sd[overall_stats$trial_condition == "dilated"], 3), 
    ", n =", overall_stats$n[overall_stats$trial_condition == "dilated"], ")\n")
cat("constricted condition: M =", round(overall_stats$mean[overall_stats$trial_condition == "constricted"], 3), 
    "(SD =", round(overall_stats$sd[overall_stats$trial_condition == "constricted"], 3), 
    ", n =", overall_stats$n[overall_stats$trial_condition == "constricted"], ")\n")
cat("t-statistic:", round(overall_t_test$statistic, 3), 
    "(df =", round(overall_t_test$parameter, 1), ")\n")
cat("p-value:", format.pval(overall_t_test$p.value, digits = 3), 
    ifelse(overall_t_test$p.value < 0.05, " (significant)", " (not significant)"), "\n")
cat("Mean difference:", round(overall_t_test$estimate[1] - overall_t_test$estimate[2], 3), "\n")
cat("Cohen's d:", round(cohens_d_overall, 3), 
    paste0("(", effect_overall, " effect)"), "\n")

# Create a formatted table for potential use in publication
formatted_table <- stat_results %>%
  mutate(
    tracker = eye_tracker,
    condition = paste0("dilated: ", sprintf("%.3f (%.3f)", dilated_mean, dilated_sd), 
                       "\nconstricted: ", sprintf("%.3f (%.3f)", constricted_mean, constricted_sd)),
    test_results = sprintf("t(%0.1f) = %0.2f, %s", 
                           df, 
                           t_statistic, 
                           ifelse(p_value < 0.001, "p < 0.001", 
                                  ifelse(p_value < 0.01, "p < 0.01", 
                                         ifelse(p_value < 0.05, "p < 0.05", 
                                                sprintf("p = %0.3f", p_value))))),
    effect = sprintf("%0.2f (%s)", cohens_d, effect_size)
  ) %>%
  select(tracker, test_type, n_samples, condition, test_results, effect)

# Print the formatted table
cat("\n\nFormatted Table for Publication:\n")
print(formatted_table)

# Save the statistical results to a CSV file
write.csv(stat_results, "./output/std_statistical_analysis.csv", row.names = FALSE)

# Save basic statistics to CSV in the current working directory
write.csv(basic_stats, file = "./output/std_descriptive_stats.csv", row.names = FALSE)

# Print a message confirming where files were saved
cat("\nFiles saved to current working directory:", getwd(), "\n")
cat("Saved files:\n")
cat("- ./output/std_statistical_analysis.csv\n")
cat("- ./output/std_descriptive_stats.csv\n")