set.seed(1371)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shadowtext)

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

# Define colors
dark_color <- "#2C3E50"    
bright_color <- "#F4D03F"
triangle_color <- "#566CE2"  # Blue triangle color

# Prepare data for participant-level analysis
participant_changes <- df %>%
  mutate(condition = ifelse(trial_condition == "dark", "dark", "bright")) %>%
  group_by(eye_tracker, participant_id, condition) %>%
  summarize(data_loss = mean(data_loss, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = condition, values_from = data_loss) %>%
  mutate(
    change = bright - dark,
    improved = bright < dark  # For data loss, lower value means better performance
  ) %>%
  mutate(improved = factor(improved, levels = c(TRUE, FALSE)))

# Get alphabetically ordered list of eye trackers
eye_tracker_order <- sort(unique(df$eye_tracker))

# Reshape data to long format for line connections
participant_long <- participant_changes %>%
  pivot_longer(
    cols = c(dark, bright),
    names_to = "condition",
    values_to = "data_loss"
  ) %>%
  mutate(condition = factor(condition, levels = c("bright", "dark")))

# Calculate summary statistics with both mean and median statistics
summary_stats <- participant_long %>%
  group_by(eye_tracker, condition) %>%
  summarize(
    median_data_loss = median(data_loss, na.rm = TRUE),
    mean_data_loss = mean(data_loss, na.rm = TRUE),
    sd_data_loss = sd(data_loss, na.rm = TRUE),
    q1 = quantile(data_loss, 0.25, na.rm = TRUE),
    q3 = quantile(data_loss, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mean_label = sprintf("%.2f ± %.2f", mean_data_loss, sd_data_loss),
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
            aes(x = condition, y = data_loss)) +
  # Add lines colored by improvement
  geom_line(aes(color = improved, group = participant_id), 
            size = 0.75, alpha = 0.8) +
  
  # Add points colored by condition (dark/bright)
  geom_point(aes(fill = condition, group = participant_id), 
             size = 2, shape = 21, color = "black", stroke = 0.1) +
  
  # Add error bars with interquartile range (Q1 to Q3)
  geom_errorbar(data = summary_stats,
                aes(x = position, y = median_data_loss, 
                    ymin = q1, ymax = q3, group = 1),
                width = 0.08, size = 0.7, color = "#063D51") +
  
  # Add smaller dots for medians with condition-dependent offset
  geom_point(data = summary_stats, 
             aes(x = position, y = median_data_loss, group = 1),
             size = 2.5, shape = 21, fill = "#FFFFFF", color = "#063D51", stroke = 0.8) +
  
  # Add mean markers (using triangles with blue fill, smaller size 1.75)
  geom_point(data = summary_stats, 
             aes(x = position, y = mean_data_loss, group = 1),
             size = 1.75, shape = 24, fill = triangle_color, color = "black", stroke = 0.8) +
  
  # Add rotated text labels for mean±std with custom positioning
  geom_shadowtext(data = summary_stats, 
                  aes(x = text_position, 
                      y = ifelse(eye_tracker == "EyeLink 1000 Plus", mean_data_loss + 0.3, mean_data_loss), 
                      label = mean_label),
                  vjust = 0.5,
                  hjust = 0.5,
                  size = 4.5,  # Increased from 3 to 4.5
                  angle = 90,
                  color = "black",
                  bg.color = "white",
                  bg.r = 0.15) +
  
  # Facet by eye tracker, alphabetically ordered
  facet_wrap(~ factor(eye_tracker, levels = eye_tracker_order), 
             ncol = 5) +
  
  # Set colors for improvement lines
  scale_color_manual(values = improvement_colors,
                     labels = c("TRUE" = "Better Data Loss in Bright", 
                                "FALSE" = "Better Data Loss in Dark")) +
  
  # Set fill colors for points (dark/bright)
  scale_fill_manual(values = c("dark" = dark_color, "bright" = bright_color),
                    guide = "none") +
  
  # Set x-axis labels to capitalized
  scale_x_discrete(labels = c("bright" = "Bright", "dark" = "Dark")) +
  
  # Format y-axis with explicit limits to prevent label clipping
  scale_y_continuous(labels = function(x) sprintf("%.1f", x), 
                     expand = expansion(mult = c(0.1, 0.05))) +
  
  # Adjust labels
  labs(
    x = "",
    y = "Data Loss (%)", 
    color = ""
  ) +
  
  # Custom theme with increased font sizes and better margins
  theme_minimal() +
  theme(
    strip.text = element_text(size = 16, face = "plain", margin = margin(b = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 15)),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(size = 14, face = "plain", color = "black"),
    axis.text.y = element_text(size = 14, margin = margin(r = 8), vjust = 0.3),
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.margin = margin(t = 15),
    panel.grid.major.y = element_line(color = "#EEEEEE", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.margin = margin(25, 25, 40, 35),
    axis.ticks.y = element_line(size = 0.3),
    axis.ticks.length.y = unit(0.2, "cm")
  )

# Print and save the plot
print(p)
ggsave("./output/participant_data_loss_changes_bright_to_dark.png", 
       plot = p, width = 12, height = 7.5, dpi = 300, bg = "white", 
       device = "png", limitsize = FALSE)