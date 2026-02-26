################################################################################
# North Carolina State University. Mali Lab 
# Spotted Turtle (Clemmys guttata) Home Range Literature Review
# Abigail Conklin, Eliza Foli, Will Harrod, Courtney Grubbs, Alex Acker, 
# Charlotte Whorton, & Ivana Mali 
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
  mutate(sex.study.id = as.factor(case_when(male == 0 ~ paste0(study.id, "F"),
                              male == 1 ~ paste0(study.id, "M"))),
         sex = as.factor(case_when(male == 0 ~ "F", male == 1 ~ "M")),
         study.id = as.factor(study.id)) %>% 
  select(-male) %>%
  distinct()

# View
glimpse(lit_rev)
slice_head(lit_rev, n = 30)

################################################################################
# 3: Explore Trends ############################################################
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
out <- rma(yi, vi, 
           mods = ~ sex + ecoregion + kde, 
           data = dat_eff,
           method = "REML") # Restricted Maximum Likelihood is standard

# View the results
sum_out <- summary(out)
sum_out

################################################################################
# 5: Visualize results #########################################################
################################################################################

# Convert output to a tibble 
out_tbl <- tibble(parameter = names(coef(out)),
                  true.val = c(beta0, beta_male, beta_eco_reg[4], beta_eco_reg[3], beta_eco_reg[2], beta_kde),
                  mean = sum_out$beta[,1],
                  ci.lb = sum_out$ci.lb,
                  ci.ub = sum_out$ci.ub,
                  p.val = sum_out$pval) %>% 
  # Subtract model estimates from the true values
  mutate(mu.diff = mean - true.val,
         lb.diff = ci.lb -true.val,
         ub.diff = ci.ub - true.val) %>% 
  # Add a column for whether or not the true parameter falls within the 95% CI
  mutate(overlap = as.factor(case_when(lb.diff * ub.diff >= 0 ~ "No",
                                       lb.diff * ub.diff < 0 ~ "Yes")),
         signif = as.factor(case_when(p.val > 0.01 ~ "No",
                                           p.val <= 0.01 ~ "Yes"))) %>% 
  # Put parameters names in an order that makes sense
  mutate(parameter = case_when(parameter == "intrcpt" ~ "Intercept",
                               parameter == "sexM" ~ "Sex Male",
                               parameter == "kde1" ~ "KDE",
                               TRUE ~ parameter)) %>% 
  mutate(parameter = factor(parameter, levels = c("Intercept", "Sex Male", "KDE",
                                                    "ecoregion1", "ecoregion2", "ecoregion3"))) %>% 
  arrange(parameter)

# View
out_tbl

# Plot the model estimates
out_tbl %>%
  # Open the plot
  ggplot(aes(y = fct_rev(parameter))) +
  # Add points at the mean values for each parameters
  geom_point(aes(x = mean, color = signif), shape = 15, size = 4) +
  # Add whiskers for 95% Bayesian Credible intervals
  geom_linerange(aes(xmin = ci.lb, xmax = ci.ub, color = signif), linewidth = 1.5) +
  # Add a vertical Line at zero
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
  # Change the Labels
  labs(x = "parameter estimate", 
       y = "", 
       title = "") + 
  # Simple theme
  theme_classic() +
  # Custom colors
  scale_color_manual(values = c("No" = "lightsteelblue4", 
                                "Yes" = "seagreen")) +
  # Edit theme
  theme(legend.position = "none",
        plot.title = element_text(size = 20, family = "Open Sans"),
        axis.text.y = element_text(size = 16, family = "Open Sans"),
        axis.title.x = element_text(size = 18, family = "Open Sans"),
        axis.text.x = element_text(size = 18, family = "Open Sans")) 


# Compare model estimates to the true results 
out_tbl %>%
  # Open the plot
  ggplot(aes(y = fct_rev(parameter))) +
  # Add points at the mean values for each parameters
  geom_point(aes(x = mu.diff, color = overlap), shape = 15, size = 4) +
  # Add whiskers for 95% Bayesian Credible intervals
  geom_linerange(aes(xmin = lb.diff, xmax = ub.diff, color = overlap), linewidth = 1.5) +
  # Add a vertical Line at zero
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
  # Change the Labels
  labs(x = "deviation from true parameter value", 
       y = "", 
       title = "") + 
  # Simple theme
  theme_classic() +
  # Custom colors
  scale_color_manual(values = c("No" = "lightsteelblue4", 
                                "Yes" = "navyblue")) +
  # Edit theme
  theme(legend.position = "none",
        plot.title = element_text(size = 20, family = "Open Sans"),
        axis.text.y = element_text(size = 16, family = "Open Sans"),
        axis.title.x = element_text(size = 18, family = "Open Sans"),
        axis.text.x = element_text(size = 18, family = "Open Sans")) 
  