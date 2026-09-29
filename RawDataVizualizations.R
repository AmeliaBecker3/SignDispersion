library(here)
library(ggplot2)

# check if medians & shoulder_scaled exist; if not, run DataWrangling.R

if (!exists("medians") || !exists("shoulder_scaled")) {
  source(here("scripts", "DataWrangling.R"))
}

# create custom theme for ggplot2 
my_theme <- function() {
  theme_minimal() +
    theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      plot.title = element_text(size = 16, face = "bold"),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    ) +
    # make background white
    theme(panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white")) +
    # keep only gridlines where there is a major tick mark
    theme(panel.grid.major = element_line(size = 0.5),
          panel.grid.minor = element_blank())
}

# plots for showing normalization process ----

# create a df that is just one single row that is a middle frame of a random single Response.ID

single_frame <- shoulder_scaled %>%
  group_by(Response.ID) %>%
  slice(floor(n() / 2)) %>%
  ungroup() %>%
  sample_n(1)

# create a plot with: nose_x and nose_y as red points, right_shoulder_x and y and 
# left_shoulder_x and y as red x, dominant_x and dominant_y as red circles

ggplot(single_frame) +
  #raw points
  geom_point(aes(x = noseX, y = noseY), color = "red", size = 6) +
  geom_point(aes(x = rshoulderX, y = rshoulderY), color = "red", shape = 4, size = 6, 
             # make the shape bold face
             stroke = 2
  ) +
  geom_point(aes(x = lshoulderX, y = lshoulderY), color = "red", shape = 4, size = 6, stroke = 2) +
  geom_point(aes(x = dominant_x, y = dominant_y), color = "red", shape = 1, size = 6, stroke = 2) +
  xlim(0, 1) + ylim(0, 1) +
  labs(title = "Mediapipe Normalized Keypoints",
       x = "X Coordinate",
       y = "Y Coordinate") +
  my_theme() +
  # increase font size for axis labels and title
  theme(    axis.title = element_text(size = 25),
            axis.text = element_text(size = 20),
            plot.title = element_text(size = 25, face = "bold"),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 20)) +
  scale_y_continuous( # makes tight layout for y-axis (i.e. gets rid of extra space below 0%)
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_x_continuous( # makes tight layout for x-axis (i.e. gets rid of extra space below 0%)
    limits = c(0, 1.1),
    expand = c(0, 0)
  )

#after nose centering
ggplot(single_frame) +
  geom_point(aes(x = centered_nose_x, y = centered_nose_y), 
             color = "purple", size = 3, stroke = 2) +
  geom_point(aes(x = centered_right_shoulder_x, y = centered_right_shoulder_y), 
             color = "purple", shape = 4, size = 3, stroke = 2) +
  geom_point(aes(x = centered_left_shoulder_x, y = centered_left_shoulder_y), 
             color = "purple", shape = 4, size = 3, stroke = 2) +
  geom_point(aes(x = centered_dominant_x, y = centered_dominant_y), 
             color = "purple", shape = 1, size = 3, stroke = 2) +
  labs(title = "Keypoints after Nose Centering",
       x = "X Coordinate",
       y = "Y Coordinate") +
  scale_y_continuous( # makes tight layout for y-axis (i.e. gets rid of extra space below 0%)
    limits = c(-.5, 1),
    expand = c(0, 0)
  ) +
  scale_x_continuous( # makes tight layout for x-axis (i.e. gets rid of extra space below 0%)
    limits = c(-.2, 1.1),
    expand = c(0, 0)
  ) +
  my_theme() +
  # increase font size for axis labels and title
  theme(    axis.title = element_text(size = 25),
            axis.text = element_text(size = 20),
            plot.title = element_text(size = 22, face = "bold"),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 20)) 

#after shoulder scaling
ggplot(single_frame) +
  geom_point(aes(x = centered_nose_x, y = centered_nose_y), color = "black", size = 3, stroke = 2) +
  geom_point(aes(x = scaled_right_shoulder_x, y = scaled_right_shoulder_y), color = "black", shape = 4, size = 3, stroke = 2) +
  geom_point(aes(x = scaled_left_shoulder_x, y = scaled_left_shoulder_y), color = "black", shape = 4, size = 3, stroke = 2) +
  geom_point(aes(x = normal_dominant_x, y = normal_dominant_y), color = "black", shape = 1, size = 3, stroke = 2) +
  xlim(0, 1) + ylim(0, 1) +
  labs(title = "Keypoints after Shoulder Scaling",
       x = "X Coordinate",
       y = "Y Coordinate") +
  scale_y_continuous( # makes tight layout for y-axis (i.e. gets rid of extra space below 0%)
    limits = c(-.5, 1),
    expand = c(0, 0)
  ) +
  scale_x_continuous( # makes tight layout for x-axis (i.e. gets rid of extra space below 0%)
    limits = c(-.2, 1.1),
    expand = c(0, 0)
  ) +
  my_theme() +
  # increase font size for axis labels and title
  theme(    axis.title = element_text(size = 25),
            axis.text = element_text(size = 20),
            plot.title = element_text(size = 22, face = "bold"),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 20)) 

# plots by frequency ----

# calculate median split of Frequency

medians <- medians %>%
  mutate(Frequency_group = cut(Frequency, breaks = 2, labels = c("Low", "High"))) 
medians$Frequency_group <- factor(medians$Frequency_group, levels = c("High", "Low"))

# get range of Frequency_group groups Low and High
range(medians$Frequency[medians$Frequency_group == "Low"], na.rm = TRUE)
range(medians$Frequency[medians$Frequency_group == "High"], na.rm = TRUE)
# plot frequency contours
ggplot(medians, aes(x = response_median_x, y = response_median_y, color = Frequency_group, fill = Frequency_group)) +
  stat_density_2d(geom = "polygon", alpha = 0.4, contour = TRUE, bins = 3
                  ) +
  scale_fill_manual(values = c("High" = "red", "Low" = "blue")) +
  scale_color_manual(values = c("High" = "red", "Low" = "blue")) +
  coord_fixed() +
  labs(x = "Wrist X Coordinates", y = "Wrist Y Coordinates", fill = "Frequency", color = "Frequency") +
  # add overall median point
  geom_point(aes(x = overall_median_x, y = overall_median_y),
             color = "black", size = 3, shape = 4) +
  scale_x_continuous(limits = c(-.1, 0.5)) +
  scale_y_continuous(limits = c(-.1, 0.5)) +
  # remove x from legend
  guides(color = guide_legend(override.aes = list(shape = NA))) +
  my_theme() 


# recreate above plot but with facets for MajorLocation and median point for each MajorLocation
ggplot(medians, aes(x = response_median_x, y = response_median_y, color = Frequency_group, fill = Frequency_group)) +
  stat_density_2d(geom = "polygon", alpha = 0.4, contour = TRUE, bins = 2) +
  scale_fill_manual(values = c("High" = "red", "Low" = "blue")) +
  scale_color_manual(values = c("High" = "red", "Low" = "blue")) +
  coord_fixed() +
  # set axis ranges
  scale_x_continuous(limits = c(-.1, 0.5)) +
  scale_y_continuous(limits = c(-.1, 0.5)) +
  labs(x = "Wrist X Coordinates", y = "Wrist Y Coordinates", fill = "Frequency", color = "Frequency") +
  # add major location median point
  geom_point(aes(x = major_location_median_x, y = major_location_median_y), 
             color = "black", size = 3, shape = 4) +
  facet_wrap(~ MajorLocation) +
  # remove x from legend
  guides(color = guide_legend(override.aes = list(shape = NA))) +
  my_theme() 

# do the same plots for neighborhood density
medians <- medians %>%
  mutate(NeighborhoodDensity_group = cut(NeighborhoodDensity, breaks = 2, labels = c("Low", "High")))

ggplot(medians, aes(x = response_median_x, y = response_median_y, color = NeighborhoodDensity_group, fill = NeighborhoodDensity_group)) +
  stat_density_2d(geom = "polygon", alpha = 0.4, contour = TRUE, bins = 3) +
  scale_fill_manual(values = c("Low" = "purple", "High" = "hotpink")) +
  scale_color_manual(values = c("Low" = "purple", "High" = "hotpink")) +
  coord_fixed() +
  labs(x = "Wrist X Coordinates", y = "Wrist Y Coordinates", fill = "Neighborhood Density", color = "Neighborhood Density") +
  # add overall median point
  geom_point(aes(x = overall_median_x, y = overall_median_y), 
             color = "black", size = 3, shape = 4)

ggplot(medians, aes(x = response_median_x, y = response_median_y, color = NeighborhoodDensity_group, fill = NeighborhoodDensity_group)) +
  stat_density_2d(geom = "polygon", alpha = 0.4, contour = TRUE, bins = 3) +
  scale_fill_manual(values = c("Low" = "purple", "High" = "hotpink")) +
  scale_color_manual(values = c("Low" = "purple", "High" = "hotpink")) +
  coord_fixed() +
  labs(x = "Wrist X Coordinates", y = "Wrist Y Coordinates", fill = "Neighborhood Density", color = "Neighborhood Density") +
  # add overall median point
  geom_point(aes(x = major_location_median_x, y = major_location_median_y), 
             color = "black", size = 3, shape = 4) +
  facet_wrap(~ MajorLocation)

# plot wrist x and y coordinates with color gradient by y_distance_from_baseline_median; add x for overall median point
ggplot(medians, aes(x = response_median_x, y = response_median_y, color = y_distance_from_median)) +
  geom_point(alpha = 0.5) +
  scale_color_gradient2(low = "turquoise", high = "purple", midpoint = 0) +
  labs(title = "Dominant Wrist Coordinates with Y Distance from Baseline Median",
       x = "Wrist X Coordinates",
       y = "Wrist Y Coordinates",
       color = "Y Distance from Baseline Median") +
  theme_minimal() +
  # add overall median point
  geom_point(aes(x = overall_median_x, y = overall_median_y),
             color = "black", size = 3, shape = 4)

# plot relationship between frequency and distance from overall median
ggplot(medians, aes(x = Frequency, y = distance_from_overall_median)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Relationship between Frequency and Distance from Overall Median",
       x = "Frequency",
       y = "Distance from Overall Median") +
  theme_minimal() 

ggplot(medians, aes(x = Frequency, y = distance_from_major_location_median)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Relationship between Frequency and Distance from Major Location Median",
       x = "Frequency",
       y = "Distance from Major Location Median") +
  theme_minimal() + 
  facet_wrap(~ MajorLocation)

# plot contours for MajorLocation

ggplot(shoulder_scaled, aes(x = normal_dominant_x,
                            y = normal_dominant_y,
                            fill = MajorLocation)) +
  stat_density_2d(geom = "polygon",
                  aes(color = MajorLocation),
                  alpha = 0.7,
                  contour = TRUE,
                  bins = 2) +
  scale_fill_manual(values = c(
    "Body" = "black",
    "Head" = "gray",
    "Arm" = "maroon",
    "Hand" = "turquoise",
    "Neutral" = "gold")) +
  scale_color_manual(values = rep("black", 5)) +
  scale_x_continuous(limits = c(0, 0.5)) +
  scale_y_continuous(limits = c(0, 0.5)) +
  coord_fixed() + # ensures that one unit on the x-axis is the same physical length as one unit on the y-axis
  # override x and y axis labels
  labs(x = "Wrist X Coordinates",
       y = "Wrist Y Coordinates") +
  # override legend title
  labs(
    fill = "Major Location"
  ) +
  # supress color legend
  guides(color = "none") +
  my_theme() +
  scale_y_continuous( # makes tight layout for y-axis (i.e. gets rid of extra space below 0%)
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_x_continuous( # makes tight layout for x-axis (i.e. gets rid of extra space below 0%)
    limits = c(0, 1),
    expand = c(0, 0)
  )

# recreate MajorLocation contours plot with medians

ggplot(medians, aes(x = response_median_x,
                    y = response_median_y,
                    fill = MajorLocation)) +
  stat_density_2d(geom = "polygon",
                  aes(color = MajorLocation),
                  alpha = 0.7,
                  contour = TRUE,
                  bins = 2) +
  scale_fill_manual(values = c(
    "Body" = "black",
    "Head" = "gray",
    "Arm" = "maroon",
    "Hand" = "turquoise",
    "Neutral" = "gold")) +
  scale_color_manual(values = rep("black", 5)) +
  scale_x_continuous(limits = c(0, 0.5)) +
  scale_y_continuous(limits = c(0, 0.5)) +
  coord_fixed() + # ensures that one unit on the x-axis is the same physical length as one unit on the y-axis
  # override x and y axis labels
  labs(x = "Median Wrist X Coordinates",
       y = "Median Wrist Y Coordinates") +
  # override legend title
  labs(
    fill = "Major Location"
  ) +
  # supress color legend
  guides(color = "none") +
  my_theme() +
  scale_y_continuous( # makes tight layout for y-axis (i.e. gets rid of extra space below 0%)
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_x_continuous( # makes tight layout for x-axis (i.e. gets rid of extra space below 0%)
    limits = c(0, 1),
    expand = c(0, 0)
  )