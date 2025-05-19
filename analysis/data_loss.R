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

# Create summary statistics table
summary_table <- df %>%
  group_by(eye_tracker, trial_condition) %>%
  summarize(
    Mean = mean(data_loss, na.rm = TRUE),
    Median = median(data_loss, na.rm = TRUE),
    SD = sd(data_loss, na.rm = TRUE),
    Min = min(data_loss, na.rm = TRUE),
    Max = max(data_loss, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Round numeric columns to 2 decimal places
  mutate(across(c(Mean, Median, SD, Min, Max), ~round(., 2)))

# Reorder the eye trackers for better presentation
ordered_trackers <- c("EyeLink 1000 Plus", "Pupil Core", "Pupil Neon", "SMI ETG", "Tobii Glasses 2")
summary_table$eye_tracker <- factor(summary_table$eye_tracker, levels = ordered_trackers)

# Sort the table by eye_tracker and trial_condition
summary_table <- summary_table %>%
  arrange(eye_tracker, trial_condition)

# Rename columns for better presentation
summary_table <- summary_table %>%
  rename(
    "Eye Tracker" = eye_tracker,
    "Condition" = trial_condition
  )

# Print the table in console
print(summary_table)

# Export as CSV
write.csv(summary_table, "./output/data_loss_table.csv", row.names = FALSE)