set.seed(1371)
# Load required libraries
library(tidyverse)
library(ggplot2)
# Load the data
path <- file.path("..", "quality_metrics", "accuracy.csv")
accuracy_df <- read_csv(path, show_col_types = FALSE)
# Define colors as specified
dark_color <- "#2C3E50"    
bright_color <- "#F4D03F"
# Prepare data for participant-level analysis
participant_changes <- accuracy_df %>%
  mutate(condition = ifelse(trial_condition == "dark", "dark", "bright")) %>%
  group_by(eye_tracker, participant_id, condition) %>%
  summarize(accuracy = mean(accuracy, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = condition, values_from = accuracy) %>%
  mutate(
    change = bright - dark,
    improved = bright < dark  # Lower accuracy value means better performance
  ) %>%
  # Convert improved to a factor with specific order
  mutate(improved = factor(improved, levels = c(TRUE, FALSE)))

# Get alphabetically ordered list of eye trackers
eye_tracker_order <- sort(unique(accuracy_df$eye_tracker))
# Reshape data to long format for line connections
participant_long <- participant_changes %>%
  pivot_longer(
    cols = c(dark, bright),
    names_to = "condition",
    values_to = "accuracy"
  ) %>%
  # Reverse the order of factor levels for condition (bright→dark)
  mutate(condition = factor(condition, levels = c("bright", "dark")))

# Calculate summary statistics with boxplot statistics (median and IQR)
summary_stats <- participant_long %>%
  group_by(eye_tracker, condition) %>%
  summarize(
    mean_accuracy = median(accuracy, na.rm = TRUE),  # Changed to median
    q1 = quantile(accuracy, 0.25, na.rm = TRUE),     # First quartile
    q3 = quantile(accuracy, 0.75, na.rm = TRUE),     # Third quartile
    .groups = "drop"
  ) %>%
  # Add condition-dependent offset
  mutate(
    # For bright (level 1): offset to the left (-0.15)
    # For dark (level 2): offset to the right (+0.15)
    offset = ifelse(condition == "bright", -0.15, 0.15),
    # Calculate the position with the offset
    position = as.numeric(condition) + offset
  )

# Set color palette for improvement lines
improvement_colors <- c("TRUE" = "#F4D03F", "FALSE" = "#2C3E50")
# Create the improved plot
p <- ggplot(participant_long, 
            aes(x = condition, y = accuracy, 
                group = participant_id)) +
  # Add lines colored by improvement
  geom_line(aes(color = improved), 
            size = 0.75, alpha = 0.8) +
  
  # Add points colored by condition (dark/bright)
  geom_point(aes(fill = condition), 
             size = 2, shape = 21, color = "black", stroke = 0.1) +
  
  # Plot individual participant points first
  geom_point(aes(fill = condition), 
             size = 2, shape = 21, color = "black", stroke = 0.1) +
  
  # Add error bars with interquartile range (Q1 to Q3)
  geom_errorbar(data = summary_stats,
                aes(x = position, y = mean_accuracy, 
                    ymin = q1, ymax = q3, group = 1),
                width = 0.08, size = 0.7, color = "#063D51") +
  
  # Add smaller dots for medians with condition-dependent offset
  geom_point(data = summary_stats, 
             aes(x = position, y = mean_accuracy, group = 1),
             size = 2.5, shape = 21, fill = "#FFFFFF", color = "#063D51", stroke = 0.8) +
  
  # Facet by eye tracker, alphabetically ordered
  facet_wrap(~ factor(eye_tracker, levels = eye_tracker_order), 
             ncol = 5) +  # Removed scales="free_y" to use same axes
  
  # Set colors for improvement lines
  scale_color_manual(values = improvement_colors,
                     labels = c("TRUE" = "Better Accuracy in Bright", 
                                "FALSE" = "Better Accuracy in Dark")) +
  
  # Set fill colors for points (dark/bright) with corrected legend labels
  scale_fill_manual(values = c("dark" = dark_color, "bright" = bright_color),
                    guide = "none") +
  
  # Set x-axis labels to capitalized
  scale_x_discrete(labels = c("bright" = "Bright", "dark" = "Dark")) +
  
  # Set fixed y-axis limits from 0 to 5.2 for all facets
  # scale_y_continuous(limits = c(0, 5.2), expand = expansion(mult = c(0, 0.1))) +
  
  # Adjust labels
  labs(
    #title = "Change in Accuracy from Bright to Dark Conditions",
    x = "",
    y = "Accuracy (deg)", 
    color = ""
  ) +
  
  # Custom elegant theme
  theme_minimal() +
  theme(
    #plot.title = element_text(face = "plain", size = 16, hjust = 0.5, margin = margin(b = 20)),
    strip.text = element_text(size = 14, face = "plain", margin = margin(b = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    axis.text = element_text(size = 12),
    # Changed from bold to plain for x-axis labels
    axis.text.x = element_text(size = 13, face = "plain", color = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.margin = margin(t = 15),
    panel.grid.major.y = element_line(color = "#EEEEEE", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(20, 20, 20, 20),
    plot.caption = element_text(hjust = 0, size = 11)
  )
# Print the plot
print(p)
# Save the plot
ggsave("./output/participant_accuracy_changes_bright_to_dark.png", plot = p, width = 12, height = 7, dpi = 300, bg = "white")