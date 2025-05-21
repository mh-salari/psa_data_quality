set.seed(1371)
library(tidyverse)
library(ggplot2)
library(shadowtext)

# Load the pupil size data
path <- file.path("..", "data", "pupil_size.csv")
pupil_df <- read_csv(path, show_col_types = FALSE)

# Filter out invalid data and calculate average pupil diameter
pupil_df <- pupil_df %>%
  filter(pup_diam_l > 0, !is.na(pup_diam_l),
         pup_diam_r > 0, !is.na(pup_diam_r)) %>%
  mutate(pup_diam_avg = (pup_diam_l + pup_diam_r) / 2)

# Aggregate data by participant, eye tracker, and trial condition
processed_data <- pupil_df %>%
  group_by(eye_tracker, participant_id, trial_condition) %>%
  summarize(mean_pupil = mean(pup_diam_avg, na.rm = TRUE), .groups = "drop")

# Define colors for conditions
dark_color <- "#2C3E50"    
bright_color <- "#F4D03F"
triangle_color <- "#566CE2"  # Blue triangle color for means

# Prepare data for participant-level analysis
participant_changes <- processed_data %>%
  mutate(condition = ifelse(trial_condition == "dark", "dark", "bright")) %>%
  group_by(eye_tracker, participant_id, condition) %>%
  summarize(pupil_size = mean(mean_pupil, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = condition, values_from = pupil_size) %>%
  mutate(
    change = bright - dark,
    expected_response = bright < dark
  ) %>%
  mutate(expected_response = factor(expected_response, levels = c(TRUE, FALSE)))

# Get alphabetically ordered list of eye trackers
eye_tracker_order <- sort(unique(processed_data$eye_tracker))

# Reshape data to long format for line connections
participant_long <- participant_changes %>%
  pivot_longer(
    cols = c(dark, bright),
    names_to = "condition",
    values_to = "pupil_size"
  ) %>%
  mutate(condition = factor(condition, levels = c("bright", "dark")))

# Calculate summary statistics with mean, median, and standard deviation
summary_stats <- participant_long %>%
  group_by(eye_tracker, condition) %>%
  summarize(
    mean_pupil = mean(pupil_size, na.rm = TRUE),
    median_pupil = median(pupil_size, na.rm = TRUE),
    q1 = quantile(pupil_size, 0.25, na.rm = TRUE),
    q3 = quantile(pupil_size, 0.75, na.rm = TRUE),
    sd_pupil = sd(pupil_size, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Add spaces before and after the ± symbol
    mean_label = sprintf("%.2f ± %.2f", mean_pupil, sd_pupil),
    offset = ifelse(condition == "bright", -0.15, 0.15),
    position = as.numeric(condition) + offset,
    # Create a separate text position that's further outward from the triangle
    text_position = ifelse(condition == "bright", 
                           as.numeric(condition) - 0.3,  # Move bright text further left
                           as.numeric(condition) + 0.3)  # Move dark text further right
  )

# Set color palette for expected response lines
response_colors <- c("TRUE" = "#F4D03F", "FALSE" = "#2C3E50")

# Create the plot
p <- ggplot(participant_long, 
            aes(x = condition, y = pupil_size)) +
  # Add lines colored by expected response
  geom_line(aes(color = expected_response, group = participant_id), 
            size = 0.75, alpha = 0.8) +
  
  # Add points colored by condition (dark/bright)
  geom_point(aes(fill = condition, group = participant_id), 
             size = 2, shape = 21, color = "black", stroke = 0.1) +
  
  # Add error bars with interquartile range (Q1 to Q3)
  geom_errorbar(data = summary_stats,
                aes(x = position, y = median_pupil, 
                    ymin = q1, ymax = q3, group = 1),
                width = 0.08, size = 0.7, color = "#063D51") +
  
  # Add smaller dots for medians with condition-dependent offset
  geom_point(data = summary_stats, 
             aes(x = position, y = median_pupil, group = 1),
             size = 2.5, shape = 21, fill = "#FFFFFF", color = "#063D51", stroke = 0.8) +
  
  # Add mean markers (using triangles with blue fill)
  geom_point(data = summary_stats, 
             aes(x = position, y = mean_pupil, group = 1),
             size = 1.75, shape = 24, fill = triangle_color, color = "black", stroke = 0.8) +
  
  # Add rotated text labels for mean±std with custom positioning
  geom_shadowtext(data = summary_stats, 
                  aes(x = text_position, y = mean_pupil, label = mean_label),
                  vjust = 0.5,  # Center vertically on the triangle
                  hjust = 0.5,  # Center text on its position
                  size = 4.5,   # Increased from 3 to 4.5
                  angle = 90,   # Rotated 90 degrees
                  color = "black",
                  bg.color = "white",
                  bg.r = 0.15) +
  
  # Facet by eye tracker, alphabetically ordered
  facet_wrap(~ factor(eye_tracker, levels = eye_tracker_order), 
             ncol = 5) +
  
  # Set colors for response lines
  scale_color_manual(values = response_colors,
                     labels = c("TRUE" = "Smaller Pupil Diameter in Bright", 
                                "FALSE" = "Larger Pupil Diameter in Bright")) +
  
  # Set fill colors for points (dark/bright)
  scale_fill_manual(values = c("dark" = dark_color, "bright" = bright_color),
                    guide = "none") +
  
  # Set x-axis labels to capitalized
  scale_x_discrete(labels = c("bright" = "Bright", "dark" = "Dark")) +
  
  # Adjust labels (caption removed)
  labs(
    x = "",
    y = "Pupil Diameter (mm)", 
    color = ""
  ) +
  
  # Custom theme with increased font sizes
  theme_minimal() +
  theme(
    strip.text = element_text(size = 16, face = "plain", margin = margin(b = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(size = 14, face = "plain", color = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
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

ggsave("./output/participant_pupil_diameter_changes_bright_to_dark.png", 
       plot = p, width = 12, height = 7, dpi = 300, bg = "white")