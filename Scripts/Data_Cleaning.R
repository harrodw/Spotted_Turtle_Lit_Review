################################################################################
# North Carolina State University. Mali Lab 
# Spotted Turtle (Clemmys guttata) Home Range Liturature Review
# Abigale Conklin, Eliza Foli, Will Harrod, Courtney Grubbs, Alex Acker, 
# Charlette Whorton, & Ivana Malli 
# Script 1 of 4: Data Preparation
################################################################################

# 1: Add data and packages #####################################################

# 1.1: Clear environments ------------------------------------------------------
rm(list = ls())

# 1.2: Add Packages ------------------------------------------------------------

# install.packages(c("tidyverse"))
library(tidyverse)

################################################################################
# 2 Paper summary stats
################################################################################

# ------------------------------------------------------------------------------
# 2.1 Summary stats for Bachran et al 2024- Evaluating the Relationship between 
# Injuries and Home-Range Size in the Endangered Spotted Turtle (Clemmys guttata)

# Read in the data 
hr1 <- read.csv("Data\\bachran_etal_2024_HRdata.csv")
# View
hr1
glimpse(hr1)

# summary stats (breeding season)
hr1 %>% 
  select(Sex, Breeding.50..KDE, Breeding.95..MCP) %>% 
  filter(if_all(everything(), ~ .x != "-")) %>%
  mutate(across(-Sex, as.numeric)) %>% 
  group_by(Sex) %>% 
  reframe(Mean.KDE = mean(Breeding.50..KDE), sd.KDE = sd(Breeding.50..KDE),
          Mean.MCP = mean(Breeding.95..MCP), sd.MCP = sd(Breeding.95..MCP))

# summary stats (non breeding season)
hr1 %>% 
  select(Sex, Non.breeding.50..KDE, Non.breeding.95..MCP) %>% 
  filter(if_all(everything(), ~ .x != "-")) %>%
  mutate(across(-Sex, as.numeric)) %>% 
  group_by(Sex) %>% 
  reframe(Mean.KDE = mean(Non.breeding.50..KDE), sd.KDE = sd(Non.breeding.50..KDE),
          Mean.MCP = mean(Non.breeding.95..MCP), sd.MCP = sd(Non.breeding.95..MCP))



