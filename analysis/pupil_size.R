set.seed(1371)

library(ggplot2)
library(dplyr)
library(tidyr)
library(shadowtext)  # For text with borders

# Read the CSV file
path <-  file.path("..", "data", "pupil_size.csv")
df <- read.csv(path)



# Process data
processed_data <- data.frame()

# Filter out invalid data
df <- df[df$pup_diam_l > 0 & !is.na(df$pup_diam_l) &
           df$pup_diam_r > 0 & !is.na(df$pup_diam_r), ]

# Calculate average pupil size
df$pup_diam_avg <- (df$pup_diam_l + df$pup_diam_r) / 2

# Aggregate data by participant, eye tracker, and trial type
for (eye_tracker in unique(df$eye_tracker)) {
  tracker_df <- df[df$eye_tracker == eye_tracker, ]
  
  for (participant in unique(tracker_df$participant_id)) {
    part_data <- tracker_df[tracker_df$participant_id == participant, ]
    
    for (trial_condition in c('dilated', 'constricted')) {
      condition_data <- part_data[part_data$trial_condition == trial_condition, ]
      
      if (nrow(condition_data) > 0) {
        new_row <- data.frame(
          participant_id = participant,
          eye_tracker = eye_tracker,
          trial_condition = trial_condition,
          mean_pupil = mean(condition_data$pup_diam_avg)
        )
        processed_data <- rbind(processed_data, new_row)
      }
    }
  }
}

# Rename our result dataframe
result_df <- processed_data

# Calculate summary statistics for labels
summary_stats <- result_df %>%
  group_by(eye_tracker, trial_condition) %>%
  summarize(
    mean_value = mean(mean_pupil, na.rm = TRUE),
    median_value = median(mean_pupil, na.rm = TRUE),
    .groups = "drop"
  )

# Define colors
dilated_color <- "#2C3E50"    
constricted_color <- "#F4D03F"  

# Create publication-quality plot 
p <- ggplot(result_df, aes(x = eye_tracker, y = mean_pupil, fill = trial_condition)) +
  # Add boxplots with alpha=0.9
  geom_boxplot(width = 0.6, alpha = 0.75, position = position_dodge(width = 0.75),
               outlier.shape = NA) +
  
  # Add individual data points
  geom_point(aes(color = trial_condition), 
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
             size = 2.5, alpha = 1, shape = 16) +
  
  # Add median line 
  stat_summary(fun = median, geom = "errorbar", 
               aes(group = trial_condition), width = 0.5, 
               position = position_dodge(width = 0.75), 
               linetype = "solid", size = 0.8, color = "#333333") +
  
  # Add direct value labels with small gray outlines
  shadowtext::geom_shadowtext(data = summary_stats,
                              aes(x = eye_tracker, y = median_value, label = sprintf("%.2f", median_value), group = trial_condition),
                              position = position_dodge(width = 0.75),
                              vjust = -0.8, size = 2.5, fontface = "bold",
                              color = "black", bg.colour = "white", bg.r = 0.2) +
  
  # Set professional color scheme
  scale_fill_manual(values = c("dilated" = dilated_color, "constricted" = constricted_color),
                    breaks = c("dilated", "constricted"),  # Explicitly set the order
                    labels = c("Dilated Condition", "Constricted Condition")) +
  scale_color_manual(values = c("dilated" = dilated_color, "constricted" = constricted_color),
                     breaks = c("dilated", "constricted"),  # Explicitly set the order
                     labels = c("Dilated Condition", "Constricted Condition")) +
  
  # Clean, concise labeling with title and subtitle
  labs(
       # title = "Pupil Size Comparison Across Eye Trackers",
       # subtitle = "dilated vs. constricted Conditions",
       x = "",
       y = "Pupil Size (mm)") +
  
  # Adjust y-axis and add horizontal guide lines
  theme(panel.grid.major.y = element_line(color = "#CCCCCC", size = 0.2)) +
  
  # Minimal theme
  theme_minimal() +
  theme(
    # Text styling
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 10, color = "black"),
    
    # Bottom legend for better space utilization
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    
    # Remove unnecessary elements
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    # Adjust plot margins
    plot.margin = margin(15, 15, 15, 15),
    
    # Transparent background
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )



# Display the plot
print(p)

# Save the plot
ggsave("./output/pupil_size.png", plot = p, width = 10, height = 7, dpi = 300, bg = "transparent")