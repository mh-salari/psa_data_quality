set.seed(1371)
library(tidyverse)
library(ggplot2)
library(shadowtext)

# Load the data
path <- file.path("..", "quality_metrics", "std.csv")
std_df <- read_csv(path, show_col_types = FALSE)

# Define colors 
dark_color <- "#2C3E50"    
bright_color <- "#F4D03F"
triangle_color <- "#566CE2"  # Blue triangle color for means

# Prepare data for participant-level analysis
participant_changes <- std_df %>%
  mutate(condition = ifelse(trial_condition == "dark", "dark", "bright")) %>%
  group_by(eye_tracker, participant_id, condition) %>%
  summarize(std = mean(std, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = condition, values_from = std) %>%
  mutate(
    change = bright - dark,
    improved = bright < dark  # Lower std value means better performance
  ) %>%
  mutate(improved = factor(improved, levels = c(TRUE, FALSE)))

# Get alphabetically ordered list of eye trackers
eye_tracker_order <- sort(unique(std_df$eye_tracker))

# Reshape data to long format for line connections
participant_long <- participant_changes %>%
  pivot_longer(
    cols = c(dark, bright),
    names_to = "condition",
    values_to = "std"
  ) %>%
  mutate(condition = factor(condition, levels = c("bright", "dark")))

# Calculate summary statistics with both mean and median statistics
summary_stats <- participant_long %>%
  group_by(eye_tracker, condition) %>%
  summarize(
    median_std = median(std, na.rm = TRUE), 
    mean_std = mean(std, na.rm = TRUE),
    sd_std = sd(std, na.rm = TRUE),
    q1 = quantile(std, 0.25, na.rm = TRUE),
    q3 = quantile(std, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mean_label = sprintf("%.2f ± %.2f", mean_std, sd_std),
    offset = ifelse(condition == "bright", -0.15, 0.15),
    position = as.numeric(condition) + offset,
    text_position = ifelse(condition == "bright", 
                           as.numeric(condition) - 0.3,
                           as.numeric(condition) + 0.3)
  )

# Set color palette for improvement lines
improvement_colors <- c("TRUE" = "#F4D03F", "FALSE" = "#2C3E50")

# Create the plot
p <- ggplot(participant_long, 
            aes(x = condition, y = std)) +
  # Add lines colored by improvement
  geom_line(aes(color = improved, group = participant_id), 
            size = 0.75, alpha = 0.8) +
  
  # Add points colored by condition (dark/bright)
  geom_point(aes(fill = condition, group = participant_id), 
             size = 2, shape = 21, color = "black", stroke = 0.1) +
  
  # Add error bars with interquartile range (Q1 to Q3)
  geom_errorbar(data = summary_stats,
                aes(x = position, y = median_std, 
                    ymin = q1, ymax = q3, group = 1),
                width = 0.08, size = 0.7, color = "#063D51") +
  
  # Add smaller dots for medians with condition-dependent offset
  geom_point(data = summary_stats, 
             aes(x = position, y = median_std, group = 1),
             size = 2.5, shape = 21, fill = "#FFFFFF", color = "#063D51", stroke = 0.8) +
  
  # Add mean markers (using triangles with blue fill)
  geom_point(data = summary_stats, 
             aes(x = position, y = mean_std, group = 1),
             size = 1.75, shape = 24, fill = triangle_color, color = "black", stroke = 0.8) +
  
  # Add rotated text labels for mean±std with custom positioning
  geom_shadowtext(data = summary_stats, 
                  aes(x = text_position, y = mean_std, label = mean_label),
                  vjust = 0.5,
                  hjust = 0.5,
                  size = 3,
                  angle = 90,
                  color = "black",
                  bg.color = "white",
                  bg.r = 0.15) +
  
  # Facet by eye tracker, alphabetically ordered
  facet_wrap(~ factor(eye_tracker, levels = eye_tracker_order), 
             ncol = 5) +
  
  # Set colors for improvement lines
  scale_color_manual(values = improvement_colors,
                     labels = c("TRUE" = "Better STD in Bright", 
                                "FALSE" = "Better STD in Dark")) +
  
  # Set fill colors for points (dark/bright)
  scale_fill_manual(values = c("dark" = dark_color, "bright" = bright_color),
                    guide = "none") +
  
  # Set x-axis labels to capitalized
  scale_x_discrete(labels = c("bright" = "Bright", "dark" = "Dark")) +
  
  # Adjust labels
  labs(
    x = "",
    y = "STD (deg)", 
    color = ""
  ) +
  
  # Custom theme
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "plain", margin = margin(b = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(size = 13, face = "plain", color = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.margin = margin(t = 15),
    panel.grid.major.y = element_line(color = "#EEEEEE", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

# Print and save the plot
print(p)
ggsave("./output/participant_std_changes_bright_to_dark.png", 
       plot = p, width = 12, height = 7, dpi = 300, bg = "white")