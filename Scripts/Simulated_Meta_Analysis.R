################################################################################
# North Carolina State University. Mali Lab 
# Spotted Turtle (Clemmys guttata) Home Range Liturature Review
# Abigale Conklin, Eliza Foli, Will Harrod, Courtney Grubbs, Alex Acker, 
# Charlette Whorton, & Ivana Malli 
# Simulating data to practice a meta analysis
################################################################################

################################################################################
# 1: Add data and packages #####################################################
################################################################################

# 1.1: Clear environments ######################################################
rm(list = ls())

# 1.2: Add Packages ############################################################

# install.packages(c("tidyverse", "metafor"))
library(tidyverse)
library(metafor)

################################################################################
# 2: Simulate Data #############################################################
################################################################################

# Set seed
set.seed(887853) # 887853 = Turtle :)

# Number of studies
n_studies <- 30

# Eco regions 
n_eco_reg <- 4
eco_reg_prob <- 1/n_eco_reg
eco_reg_vect <- rep(eco_reg_prob, n_eco_reg)

# Effect sizes 
beta0 <- 5 
beta_male <- 2
beta_kde <- 1
beta_eco_reg <- c(0, rnorm(n = n_eco_reg - 1, 0, 2))

# Study information
n_turtles_study <- rpois(n = n_studies, 15)
n_total_turtles <- sum(n_turtles_study)
ecoregion_mat <- rmultinom(n = n_studies, size = 1, prob = eco_reg_vect)
ecoregion <- colSums(row(ecoregion_mat) * ecoregion_mat)
kde <- rbinom(n_studies, prob = 0.3, size = 1)

# Random noise 
sd <- 2
eps <- rnorm(n_total_turtles, mean = 0, sd = sd)

# Simulated "raw" data sets
sim_dat <- tibble(
                  study.id = rep(seq(from = 1, to = n_studies), n_turtles_study),
                  male = rbinom(n_total_turtles, prob = 0.5, size = 1),
                  kde = rep(kde, n_turtles_study),
                  ecoregion = rep(ecoregion, n_turtles_study),
                  eps = eps
                  ) 
# View
glimpse(sim_dat)
slice_head(sim_dat, n = 30)
sim_dat %>% count(study.id, ecoregion)
sim_dat %>%  count(study.id, kde)
sim_dat %>%  count(study.id, male)

# Simulate home ranges for each turtle based on pre defined relationships
sim_dat_hr <- sim_dat %>% 
  mutate(hr = beta0 + beta_male * male + beta_kde*kde + beta_eco_reg[ecoregion] + eps) %>% 
  # make sure nothing is negative
  mutate(hr = case_when(hr > 0 ~ hr, hr < 0 ~ -hr/4, hr == 0 ~ 0.1))
# view
slice_head(sim_dat_hr, n = 30)

# Combine into a simulated literature review dataset
lit_rev <- sim_dat_hr %>% 
  group_by(study.id, male) %>% 
  reframe(hr.mean = mean(hr),
          hr.sd = sd(hr),
          n.turtles = n(),
          ecoregion = as.factor(unique(ecoregion)),
          kde = as.factor(unique(kde))) %>% 
  mutate(study.id = as.factor(case_when(male == 0 ~ paste0(study.id, "F"),
                              male == 1 ~ paste0(study.id, "M"))),
         sex = as.factor(case_when(male == 0 ~ "F", male == 1 ~ "M"))) %>% 
  select(-male) %>%
  distinct()

# View
glimpse(lit_rev)
slice_head(lit_rev, n = 30)

################################################################################
# 3: Explore Trends#############################################################
################################################################################

# Histogram of mean home range sizes by sex
lit_rev %>% 
  ggplot() +
  geom_histogram(aes(x = hr.mean, fill = sex)) +
  theme_classic()

# Histogram of mean home range sizes by ecoregion
lit_rev %>% 
  ggplot() +
  geom_histogram(aes(x = hr.mean, fill = ecoregion)) +
  theme_classic()

# Histogram of standard deviations 
lit_rev %>% 
  ggplot() +
  geom_histogram(aes(x = hr.sd), fill = "forestgreen") +
  theme_classic()

################################################################################
# 4: Meta Analysis #############################################################
################################################################################
  
# View the lit review again
glimpse(lit_rev)

# Calculate the effect size (yi) and sampling variance (vi)
dat_eff <- escalc(measure = "MN", 
                  mi = hr.mean, 
                  sdi = hr.sd, 
                  ni = n.turtles, 
                  data = lit_rev)

# View the new columns: 'yi' (effect size) and 'vi' (variance)
slice_head(dat_eff, n = 30)

# Fit the meta-regression model
res <- rma(yi, vi, 
           mods = ~ sex + ecoregion + kde, 
           data = dat_eff,
           method = "REML") # Restricted Maximum Likelihood is standard

# View the results
summary(res)
