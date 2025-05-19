set.seed(1371)

# Load required libraries
library(tidyverse)
library(ggplot2)
library(shadowtext)  # For text with borders - install if needed: install.packages("shadowtext")

# Load the data
path <-  file.path("..", "quality_metrics", "accuracy.csv")
accuracy_df <- read_csv(path, show_col_types = FALSE)

# Aggregate data by participant, eye tracker, and trial condition
# This calculates the mean accuracy across all trials of the same condition
aggregated_df <- accuracy_df %>%
  group_by(participant_id, eye_tracker, trial_condition) %>%
  summarize(accuracy = mean(accuracy, na.rm = TRUE), .groups = "drop")

# Define colors
dark_color <- "#2C3E50"    
bright_color <- "#F4D03F"  

# Calculate differences between dark and bright
delta_df <- aggregated_df %>%  # Using aggregated data
  group_by(eye_tracker, trial_condition) %>%
  summarize(accuracy = mean(accuracy, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = trial_condition, values_from = accuracy) %>%
  mutate(delta = dark - bright)

# Calculate summary statistics for labels
summary_stats <- aggregated_df %>%  # Using aggregated data
  group_by(eye_tracker, trial_condition) %>%
  summarize(
    mean_value = mean(accuracy, na.rm = TRUE),
    median_value = median(accuracy, na.rm = TRUE),
    .groups = "drop"
  )

# Create the plot
p <- ggplot(aggregated_df, aes(x = eye_tracker, y = accuracy, fill = trial_condition)) +  # Using aggregated data
  # Add boxplots
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
                              aes(x = eye_tracker, y = median_value, label = sprintf("%.3f", median_value), group = trial_condition),
                              position = position_dodge(width = 0.75),
                              vjust = -0.8, size = 2.5, fontface = "bold",
                              color = "black", bg.colour = "white", bg.r = 0.2) +
  
  # Set professional color scheme
  scale_fill_manual(values = c("dark" = dark_color, "bright" = bright_color),
                    breaks = c("dark", "bright"),  # Explicitly set the order
                    labels = c("Dark Condition", "Bright Condition")) +
  scale_color_manual(values = c("dark" = dark_color, "bright" = bright_color),
                     breaks = c("dark", "bright"),  # Explicitly set the order
                     labels = c("Dark Condition", "Bright Condition")) +
  
  # Clean, concise labeling
  labs(
    # title = "Comparison of Gaze Accuracy Across Eye Trackers",
    # subtitle = "Dark vs. Bright Conditions",
    x = "",
    y = " Accuracy (deg)") +
  
  # Adjust y-axis and add horizontal guide lines
  scale_y_continuous(limits = c(0, 8), 
                     breaks = seq(0, 8, 2),
                     expand = c(0, 0)) +
  
  # Add horizontal guide lines for easier reading
  theme(panel.grid.major.y = element_line(color = "#CCCCCC", size = 0.2)) +
  
  # Minimal theme matching PSA
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
ggsave("./output/accuracy.png", plot = p, width = 10, height = 7, dpi = 300, bg = "white")