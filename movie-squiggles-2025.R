# Movie Squiggles 2025
#
# Author: Francesco Grassi
# Date: May 2025
#
# Visualize the movies I watched so far this year as a series of "squiggles" (open spirals that cross over themselves)
# which length corresponds to the movie duration, and color to the watching platform.
# Use Perlin noise to generate wiggles in the spiral path.

# Libraries ----
library(tidyverse)
library(ambient)
library(scales)
library(ggtext)

# Function to generate squiggles ----
generate_squiggles <- function(sq_length = 10, sq_res = 3000, noise_freq = 1.7, noise_ampl = 1.2, offset = 0.2, loop_dist = 0.5) {
  # Arguments:
  # - 'sq_length': length of the squiggle (must stay < 10 for optimal results)
  # - 'sq_res': resolution of the squiggle. The higher the more points are used to draw the curve
  # - 'noise_freq': frequency of the Perlin noise. Controls how quickly the wiggles occur
  # - 'noise_ampl': amplitude of the Perlin noise. Controls how "wild" the wiggles get
  # - 'offset': offset of the squiggle starting point from (0, 0)
  # - 'loop_dist': parameter to control the distance between loops of the squiggle
  # Value:
  # - a df containing x and y coordinates of the squiggle points
  
  # Define a 1D "time" variable
  # This will give the overall length of the squiggle
  t <- seq(0, sq_length, length.out = sq_res)
  
  # Sample 1D noise along the "time" variable
  noise <- gen_perlin(t, frequency = noise_freq)
  
  # Define radius values as in an Archimedean spiral, add noise
  r <- offset + loop_dist * t + noise_ampl * noise
  
  # Calculate squiggle coordinates from radius
  df <- tibble(
    x = r * cos(2*pi * t),
    y = r * sin(2*pi * t)
  )
  
  return(df)
  
}

# Load and prepare data ----

df <- read_csv("movies-2025.csv")  # load movies dataset

# Get relevant variables (title, platform and duration) and refine
df <- df %>% 
  select(Title, Duration, Platform) %>% 
  mutate(
    Title = fct_inorder(Title),  # turn titles into factor in the order of first appearance to keep order in plotting
    Platform = case_match(
      Platform,
      "Prime" ~ "Prime Video",
      "Streaming" ~ "Other Streaming Platforms",
      .default = Platform
    )
  ) %>% 
  mutate(
    Platform = factor(Platform, levels = c("Cinema", "Apple TV", "Netflix", "Prime Video", "Other Streaming Platforms"))
  )

# Rescale movie duration to be between 2 and 10 (a good range to get nice squiggles)
df <- df %>% 
  mutate(
    dur_scaled = rescale(Duration, to = c(2, 10))
  )

# Generate squiggles ----

# Apply squiggle function to each movie, using duration to control squiggle length
squiggle_df <- pmap(
  .l = list(duration = df$dur_scaled, title = df$Title, platform = df$Platform),  # carry title and platform for plotting
  .f = function(duration, title, platform) {
    generate_squiggles(sq_length = duration) %>% 
      mutate(
        title = title,
        platform = platform
      )
  }
) %>% 
  list_rbind()

# Plotting ----

# Define color palette based on platform official colors
colors <- c(
  "#FFB238",  # cinema
  "#000000",  # apple tv
  "#E50914",  # netflix
  "#1399FF",  # prime video
  "#EA7AF4"  # other platforms
)

# Plot one squiggle per movie, color by platform, arrange in a grid
squiggle_plot <- squiggle_df %>% 
  ggplot(aes(x = x, y = y, color = platform)) +
  geom_path(linewidth = 1, alpha = 0.8) +
  scale_color_manual(values = colors) +
  coord_equal() +
  facet_wrap(~title, ncol = 7, strip.position = "bottom", labeller = label_wrap_gen(width = 25)) +
  labs(title = "My 2025 movies<br>at the <span style='color:#FFB238'>cinema</span>, on <span style='color:#000000'>Apple TV</span>, 
       <span style='color:#E50914'>Netflix</span>, <span style='color:#1399FF'>Prime Video</span> and <span style='color:#EA7AF4'>other streaming platforms</span>",
       subtitle = "Length of the squiggle represents movie duration") +
  theme_void(base_family = "Lucida Sans Unicode") +
  theme(
    legend.position = "none",
    plot.title = element_markdown(size = 26),
    plot.subtitle = element_text(size = 16),
    strip.text = element_text(size = 9),
    panel.spacing.x = unit(50, "points"),
    plot.margin = margin(0, 5, 10, 5, unit = "pt")
  )

#squiggle_plot  
ggsave("movie-squiggles-2025.png", squiggle_plot, width = 4800, height = 3000, dpi = 300, unit = "px", bg = "white")
