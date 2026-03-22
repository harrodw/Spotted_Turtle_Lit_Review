################################################################################
# North Carolina State University. Mali Lab 
# Spotted Turtle (Clemmys guttata) Home Range Liturature Review
# Abigale Conklin, Eliza Foli, Will Harrod, Courtney Grubbs, Alex Acker, 
# Charlette Whorton, & Ivana Malli 
# Estimating home ranges from raw data for:
# Angoh et al. 2021 Effects of invasive wetland macrophytes on habitat selection
# and movement by freshwater turtles
################################################################################

# 1.2: Add Packages ------------------------------------------------------------

# install.packages(c("tidyverse", "amt"))
library(tidyverse)
library(amt)
library(sf)

# 1.3: Add data ----------------------------------------------------------------
turt_points <- read.csv("Data\\turtle_tracking.csv")

# View data 
glimpse(turt_points)

# 1.4: Clean Data --------------------------------------------------------------
spottie_points <- turt_points %>% 
  filter(species == "spotted") %>% 
  dplyr::select(id, species, sex, date, time, x, y) %>% 
  mutate(id = factor(id))

# View cleaned data 
glimpse(spottie_points)

# Convert to track object
spotties_track <- make_track(spottie_points, x, y, id = id, crs = 32617) 

# View
spotties_track

# Map
spotties_track %>% 
  ggplot() +
  geom_point(aes(x = x_, y = y_, col = id, fill = id), alpha = 0.25) +
  theme_minimal()

# Make an object of turtles by sex
turtle_table <- spottie_points %>% 
  distinct(id, sex) %>% 
  arrange(id)
# View
turtle_table

################################################################################
# 2: Home ranges ###############################################################
################################################################################

# Define 95% MCP for each turtle -----------------------------------------------
spotties_mcp <- spotties_track %>% 
  nest(data = -"id") %>%
  arrange(id) %>% 
  mutate(mcp = map(data, function(x) 
    x %>% hr_mcp(levels = c(0.95)))) 

# View
spotties_mcp

# plot a turtle's hr
plot(spotties_mcp$mcp[[4]])

# Calculate HR area
mcp_area <- map_df(spotties_mcp$mcp, hr_area) %>% 
  mutate(id = turtle_table$id,
         sex = turtle_table$sex,
         # Convert to ha
         area = area / 10000)

# View
mcp_area

################################################################################
# 3: Home range summaries ######################################################
################################################################################

# Mean and sd of home range
hr_stats <- mcp_area %>% 
  group_by(sex) %>% 
  reframe(sex, 
          mean = mean(area),
          sd = sd(area),
          count = n()) %>% 
  distinct()

# View
hr_stats
