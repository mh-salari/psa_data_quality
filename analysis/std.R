set.seed(1371)
# Load required libraries
library(tidyverse)
library(ggplot2)
library(shadowtext)  # For text with borders - install if needed: install.packages("shadowtext")

# Load the std data
path <-  file.path("..", "quality_metrics", "std.csv")
std_df <- read_csv(path, show_col_types = FALSE)

# Define colors
dilated_color <- "#2C3E50"    
constricted_color <- "#F4D03F"  

# Calculate differences between dilated and constricted
delta_df <- std_df %>%
  group_by(eye_tracker, trial_condition) %>%
  summarize(std = mean(std, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = trial_condition, values_from = std) %>%
  mutate(delta = dilated - constricted)

# Calculate summary statistics for labels
summary_stats <- std_df %>%
  group_by(eye_tracker, trial_condition) %>%
  summarize(
    mean_value = mean(std, na.rm = TRUE),
    median_value = median(std, na.rm = TRUE),
    .groups = "drop"
  )

# Get maximum y value for appropriate scaling
max_std <- max(std_df$std, na.rm = TRUE)

# Create the plot
p <- ggplot(std_df, aes(x = eye_tracker, y = std, fill = trial_condition)) +
  # Add boxplots
  geom_boxplot(width = 0.6, alpha = 0.75, position = position_dodge(width = 0.75),
               outlier.shape = NA) +
  
  # Add individual data points
  geom_point(aes(color = trial_condition), 
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
             size = 2.5, alpha =1, shape = 16) +
  
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
  scale_fill_manual(values = c("dilated" = dilated_color, "constricted" = constricted_color),
                    breaks = c("dilated", "constricted"),  # Explicitly set the order
                    labels = c("Dilated Condition", "Constricted Condition")) +
  scale_color_manual(values = c("dilated" = dilated_color, "constricted" = constricted_color),
                     breaks = c("dilated", "constricted"),  # Explicitly set the order
                     labels = c("Dilated Condition", "Constricted Condition")) +
  
  # Clean, concise labeling
  labs(
       # title = "Comparison of Gaze std Across Eye Trackers",
       # subtitle = "dilated vs. constricted Conditions",
       x = "",
       y = "STD (deg)") +
  
  # Adjust y-axis and add horizontal guide lines
  scale_y_continuous(limits = c(0, max_std * 1.2), # Increased to make room for labels
                     breaks = seq(0, ceiling(max_std), 0.5),
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
ggsave("./output/std.png", plot = p, width = 10, height = 7, dpi = 300, bg = "transparent")