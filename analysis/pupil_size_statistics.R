library(dplyr)

# Read the CSV file
path <-  file.path("..", "data", "pupil_size.csv")
df <- read.csv(path)

# Process data
result_df <- data.frame()

# Filter out invalid data
df <- df[df$pup_diam_l > 0 & !is.na(df$pup_diam_l) &
           df$pup_diam_r > 0 & !is.na(df$pup_diam_r), ]

# Calculate average pupil size
df$pup_diam_avg <- (df$pup_diam_l + df$pup_diam_r) / 2



# Process the data without normalization
result_df <- data.frame()

for (eye_tracker in unique(df$eye_tracker)) {
  tracker_df <- df[df$eye_tracker == eye_tracker, ]
  
  for (participant in unique(tracker_df$participant_id)) {
    part_data <- tracker_df[tracker_df$participant_id == participant, ]
    
    for (trial_condition in c('dark', 'bright')) {
      condition_data <- part_data[part_data$trial_condition == trial_condition, ]
      
      if (nrow(condition_data) > 0) {
        new_row <- data.frame(
          participant_id = participant,
          eye_tracker = eye_tracker,
          trial_condition = trial_condition,
          pup_diam_avg = mean(condition_data$pup_diam_avg)
        )
        result_df <- rbind(result_df, new_row)
      }
    }
  }
}


# ============= BASIC DESCRIPTIVE STATISTICS ===============

# Calculate basic descriptive statistics by eye tracker and condition
basic_stats <- result_df %>%
  group_by(eye_tracker, trial_condition) %>%
  summarize(
    n = n(),
    mean = mean(pup_diam_avg, na.rm = TRUE),
    median = median(pup_diam_avg, na.rm = TRUE),
    sd = sd(pup_diam_avg, na.rm = TRUE),
    min = min(pup_diam_avg, na.rm = TRUE),
    max = max(pup_diam_avg, na.rm = TRUE),
    q25 = quantile(pup_diam_avg, 0.25, na.rm = TRUE),
    q75 = quantile(pup_diam_avg, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# Print basic statistics in a well-formatted way
cat("\n================================================================\n")
cat("Basic Descriptive Statistics of Pupil Size by Eye Tracker and Condition\n")
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
    cat("  Mean pupil size:", round(tracker_data$mean[i], 3), "mm (SD =", round(tracker_data$sd[i], 3), ")\n")
    cat("  Median pupil size:", round(tracker_data$median[i], 3), "mm\n")
    cat("  Range:", round(tracker_data$min[i], 3), "to", round(tracker_data$max[i], 3), "mm\n")
    cat("  Interquartile Range:", round(tracker_data$q25[i], 3), "to", round(tracker_data$q75[i], 3), "mm\n\n")
  }
}
  
# Save basic statistics to CSV in the current working directory
write.csv(basic_stats, file = "./output/pupil_size_descriptive_stats.csv", row.names = FALSE)