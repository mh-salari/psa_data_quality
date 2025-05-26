set.seed(1371)
# Load required libraries
library(tidyverse)
library(ggplot2)
library(shadowtext)  # For text with borders - install if needed: install.packages("shadowtext")
# Load the data
path <-  file.path("..", "quality_metrics", "apparent_gaze_shift.csv")
apparent_gaze_shift_df <- read_csv(path, show_col_types = FALSE)
# Calculate mean delta for each eye tracker
delta_df <- apparent_gaze_shift_df %>%
  group_by(eye_tracker) %>%
  summarize(delta = mean(apparent_gaze_shift, na.rm = TRUE))
# Calculate summary statistics for labels
summary_stats <- apparent_gaze_shift_df %>%
  group_by(eye_tracker) %>%
  summarize(
    mean_value = mean(apparent_gaze_shift, na.rm = TRUE),
    median_value = median(apparent_gaze_shift, na.rm = TRUE),
    .groups = "drop"
  )
# Define colors
box_color <- "#575F82"  
point_color <- "#8B7AA2"
# Create the plot - clean and simple
p <- ggplot(apparent_gaze_shift_df, aes(x = eye_tracker, y = apparent_gaze_shift)) +
  # Add boxplots
  geom_boxplot(width = 0.6, fill = box_color, alpha = 0.75,
               outlier.shape = NA, color = "#333333") +
  
  # Add individual data points
  geom_point(color = point_color, 
             position = position_jitter(width = 0.15),
             size = 2, alpha =1) +
  
  # Add median line 
  stat_summary(fun = median, geom = "errorbar", 
               aes(group = 1), width = 0.5,
               linetype = "solid", size = 0.8, color = "#333333") +
  
  # Add direct value labels with small gray outlines - increased size to 4.5
  shadowtext::geom_shadowtext(data = summary_stats,
                              aes(x = eye_tracker, y = median_value, label = sprintf("%.2f", median_value)),
                              vjust = -0.8, size = 4.5, fontface = "bold",
                              color = "black", bg.colour = "white", bg.r = 0.2) +
  
  # Clean, concise labeling
  labs(#title = "Apparent Gaze Shift",
    x = "",
    y = "Apparent Gaze Shift (deg)") +
  
  # Adjust y-axis and add horizontal guide lines
  scale_y_continuous(limits = c(0, max(apparent_gaze_shift_df$apparent_gaze_shift, na.rm = TRUE) * 1.2), # Increased to make room for labels
                     expand = c(0, 0)) +
  
  # Add horizontal guide lines for easier reading
  theme(panel.grid.major.y = element_line(color = "#CCCCCC", size = 0.2)) +
  
  # Minimal theme
  theme_minimal() +
  theme(
    # Text styling - increased font sizes to match other plots
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 14, color = "black"),
    strip.text = element_text(size = 16, face = "plain"),
    
    # Remove unnecessary elements
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    # Match settings from other plots
    panel.spacing = unit(1.5, "lines"),
    plot.margin = margin(20, 20, 20, 20),
    
    # Transparent background
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )
# Display the plot
print(p)
# Save with transparent background
ggsave("./output/apparent_gaze_shift.png", plot = p, width = 10, height = 6, dpi = 300, bg = "white")